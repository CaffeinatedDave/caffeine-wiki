package controllers

import play.api._
import play.api.mvc._
import models._
import play.api.data._
import play.api.data.Forms._
import java.net.URLDecoder._
import java.net.URL

object Application extends Controller with Secured {

  val articleForm = Form(
    mapping(
      "id" -> number,
      "title" -> text,
      "content" -> text
    )
    ((i, t, c) => new Article(i, t, c)) 
    ((article: Article) => Some((article.id.toInt, article.title, article.content))) 
  )  
  
  def index = Action {
    Ok("Wiki is up!")

  }
  def loggedInUser(request: Request[AnyContent]) : Option[User] = {
    request.session.get(Security.username) match {
      case Some(u) => User.getByUsername(u)
      case None => None
    }
  }
  
  def wiki(page: String) = Action { implicit request =>
    val sensiblePage = decode(page.head.toUpper + page.tail, "utf-8")
    Logger.info("Considering loading page for " + sensiblePage + " (was " + page + ")")
    sensiblePage match {
      case "Recent" => Redirect(routes.Application.listArticles(""))
      case "Tag" => Redirect(routes.Application.listTags)
      case p => try {
        val article: Article = Article.getArticleByName(p)
        (article.locked, article.id) match {
          case (_, -1) => Redirect(routes.Application.wikiEdit(p)).flashing("error" -> "Page doesn't exist, why not create it?")
          case (true, _) => {
            val user = loggedInUser(request)
            user match {
              case Some(_) => Ok(views.html.article(user, article))
              case None => {
                Redirect(routes.Application.wiki("")).flashing("error" -> "Forbidden")
              }
            }
          }
          case (false, _) => Ok(views.html.article(loggedInUser(request), article))
        }
      } catch {
        case anfe: ArticleNotFoundException => Redirect(routes.Application.wikiEdit(page)).flashing("error" -> anfe.smth)
      }
    }
  }

  def wikiEdit(page: String) = withUser { user => implicit request =>
    val sensiblePage = decode(page.head.toUpper + page.tail, "utf-8")
    Logger.info("Considering loading edit form for " + sensiblePage + " (was " + page + ")")
    sensiblePage match {
      case "Recent" => Redirect(routes.Application.listArticles("")).flashing("error" -> "Can't edit recent history this way...")
      case "Tag" => Redirect(routes.Application.listTags).flashing("error" -> "Can't edit tags this way...")
      case p => try {
        val article: Article = Article.getArticleByName(p)
        article.id match {
          case -1 => Ok(views.html.editArticle(user, articleForm fill (article), article)).flashing("error" -> "Page doesn't exist, why not create it?")
          case _ => Ok(views.html.editArticle(user, articleForm fill (article), article))
        }
      } catch {
        // Something here for when we have edit permissions and such 
        case anfe: ArticleNotFoundException => Redirect(routes.Application.wiki("Home")).flashing("error" -> anfe.smth)
      }
    }
  }
  
  def wikiEditSave = withUser { user => implicit request => 
    articleForm.bindFromRequest.fold(
      formWithErrors => BadRequest("I don't even know"),
      article => {
        try {
          val sensibleTitle = decode(article.title.head.toUpper + article.title.tail, "utf-8")
          article.id match {
            case -1 => Article.save(sensibleTitle, article.content)
            case _ => Article.save(article.id, sensibleTitle, article.content)
          }
          Redirect(routes.Application.wiki(article.title)).flashing("success" -> "Saved.")
        } catch {
          case rce: RenameCollisionException => BadRequest(views.html.editArticle(user, articleForm fill article, article)).flashing("error" -> rce.smth)
          case rwe: RestrictedWordException => Redirect(routes.Application.wiki("RestrictedWords")).flashing("error" -> rwe.smth)
        }
      }
    )
  }

  def wikiEditPreview = withUser { user => implicit request => 
    articleForm.bindFromRequest.fold(
      formWithErrors => BadRequest("I don't even know"),
      article => {
        val sensibleTitle = decode(article.title.head.toUpper + article.title.tail, "utf-8")
        Ok(views.html.editArticle(user, articleForm fill (article), article))
      }
    )
  }
  
  def delete(name: String) = withUser { user => implicit request =>
    val sensiblePage = decode(name.head.toUpper + name.tail, "utf-8")
    Logger.info("Preparing deletion form for " + sensiblePage + " (was " + name + ")")
    Article.getArticleByName(sensiblePage) match {
      case a: Article if (a.id == -1) => Redirect(routes.Application.wiki("Home")).flashing("error" -> "Can't delete " .+ (sensiblePage) .+ (". Page not found."))
      case a: Article => Ok(views.html.confirmDelete(user, a))
    }
  }
  
  def confirmDelete = withUser { user => implicit request =>
    try {
      val id = request.body.asFormUrlEncoded.get("id")(0)
      Article.removeArticleById(id.toLong)
      Redirect(routes.Application.wiki("Home")).flashing("success" -> "Removed.")
    } catch {
      case snee: NoSuchElementException => Redirect(routes.Application.wiki("Home")).flashing("error" -> "This isn't valid.")
      case nfe: NumberFormatException => Redirect(routes.Application.wiki("Home")).flashing("error" -> "Not a number, stop fucking with my forms.")
      // Not a fan of generics, but I hate exception screens more...
      case e: Throwable => Redirect(routes.Application.wiki("Home")).flashing("error" -> e.getMessage())
    }
  }
  
  def listRecent = Action { implicit request =>
    val user = loggedInUser(request)
    val articleList = user match {
      case Some(_) => Article.getAll(true)
      case _ => Article.getAll(false)
    }
    Ok(views.html.articleList(user, articleList, 'all, ""))
  }
  
  def listArticles(search: String = "") = Action { implicit request =>
    val user = loggedInUser(request)
    decode(search, "utf-8") match {
      case "" => Redirect(routes.Application.listRecent)
      case x  => {
        user match {
          case Some(_) => Ok(views.html.articleList(user, Tag.getArticlesWithTag(x, true), 'tag, x))
          case None => Ok(views.html.articleList(user, Tag.getArticlesWithTag(x, false), 'tag, x))
        }
      }
    }
  }
  
  def listTags = Action { implicit request =>
    val user = loggedInUser(request)
    val tagList = user match {
      case Some(_) => Tag.getAllTags(true)
      case None => Tag.getAllTags(false)
    }
    Ok(views.html.tags(user, tagList))
  }
  
  def deleteTag(tag: String) = withUser { user => implicit request =>
    Tag.deleteTag(tag) match {
      case true => Redirect(routes.Application.listTags).flashing("success" -> ("Tag " + tag + " deleted."))
      case false => Redirect(routes.Application.listTags).flashing("error" -> ("Tag " + tag + " cannot be removed."))
    }
  }
}