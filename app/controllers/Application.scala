package controllers

import play.api._
import play.api.mvc._
import models._
import play.api.data._
import play.api.data.Forms._

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
    val article: Article = Article.getArticleByName(page)
    Ok(views.html.article(article))
  }

  def wikiEdit(page: String) = Action { implicit request =>
    val article: Article = Article.getArticleByName(page)
    Ok(views.html.editArticle(articleForm fill (article), article))
  }
  
  def wikiEditSave = Action { implicit request => 
    articleForm.bindFromRequest.fold(
      formWithErrors => BadRequest("I don't even know"),
      article => {
        article.id match {
          case -1 => Article.save(article.title, article.content)
          case _ => Article.save(article.id, article.title, article.content)
        }
        Redirect(routes.Application.wiki(article.title))
      }
    )
  }

}