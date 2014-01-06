package controllers

import play.api._
import play.api.mvc._
import models._
import play.api.data._
import play.api.data.Forms._
import java.net.URLDecoder._

object Application extends Controller {

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

  def wiki(page: String) = Action { implicit request =>
    val sensiblePage = decode(page.head.toUpper + page.tail, "utf-8")
    Logger.info("Considering loading page for " + sensiblePage + " (was " + page + ")")
    sensiblePage match {
      case "Recent" => Redirect(routes.Application.showRecent)
      case p => try {
        val article: Article = Article.getArticleByName(p)
        article.id match {
          case -1 => Redirect(routes.Application.wikiEdit(p)).flashing("error" -> "Page doesn't exist, why not create it?")
          case _ => Ok(views.html.article(article))
        }
      } catch {
        case anfe: ArticleNotFoundException => Redirect(routes.Application.wikiEdit(page)).flashing("error" -> anfe.smth)
      }
    }
  }

  def wikiEdit(page: String) = Action { implicit request =>
    val sensiblePage = decode(page.head.toUpper + page.tail, "utf-8")
    Logger.info("Considering loading edit form for " + sensiblePage + " (was " + page + ")")
    sensiblePage match {
      case "Recent" => Redirect(routes.Application.showRecent).flashing("error" -> "Can't edit recent history this way...")
      case p => try {
        val article: Article = Article.getArticleByName(p)
        article.id match {
          case -1 => Ok(views.html.editArticle(articleForm fill (article), article)).flashing("error" -> "Page doesn't exist, why not create it?")
          case _ => Ok(views.html.editArticle(articleForm fill (article), article))
        }
      } catch {
        // Something here for when we have edit permissions and such 
        case anfe: ArticleNotFoundException => Redirect(routes.Application.wiki("Home")).flashing("error" -> anfe.smth)
      }
    }
  }
  
  def wikiEditSave = Action { implicit request => 
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
          case rce: RenameCollisionException => BadRequest(views.html.editArticle(articleForm fill article, article)).flashing("error" -> rce.smth)
          case rwe: RestrictedWordException => Redirect(routes.Application.wiki("RestrictedWords")).flashing("error" -> rwe.smth)
        }
      }
    )
  }
  
  def delete(name: String) = Action { implicit request =>
    val sensiblePage = decode(name.head.toUpper + name.tail, "utf-8")
    Logger.info("Preparing deletion form for " + sensiblePage + " (was " + name + ")")
    Article.getArticleByName(sensiblePage) match {
      case a: Article if (a.id == -1) => Redirect(routes.Application.wiki("Home")).flashing("error" -> "Can't delete " .+ (sensiblePage) .+ (". Page not found."))
      case a: Article => Ok(views.html.confirmDelete(a))
    }
  }
  
  def confirmDelete = Action { implicit request =>
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
  
  def showRecent = Action { implicit request =>
    val articleList = Article.getAll
    Ok(views.html.recent(articleList))
  }

}