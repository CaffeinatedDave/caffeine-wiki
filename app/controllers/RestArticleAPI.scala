package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import models._
import play.api.data._
import play.api.data.Forms._
import java.net.URLDecoder._

object RestArticleAPI extends Controller {
  implicit val rds = (
    (__ \ 'id).read[Long]
  )

  def changeLock = Action(parse.json) { request =>
    request.body.validate[(Long)].map{ 
      case (id) => {
        Ok(Article.lockArticle(id))
      }
    }.recoverTotal{
      e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
    }
  }
  }