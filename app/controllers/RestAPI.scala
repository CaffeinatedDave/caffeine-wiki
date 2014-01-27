package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import models._
import play.api.data._
import play.api.data.Forms._
import java.net.URLDecoder._

object RestAPI extends Controller {
  implicit val rds = (
    (__ \ 'id).read[Long] and
    (__ \ 'tag).read[String]
  ) tupled

  def addTag = Action(parse.json) { request =>
    
    request.body.validate[(Long, String)].map{ 
      case (id, tag) => {
        Tag.addTagToArticle(id, tag)
        Logger.info("Tagging article " + " with " + tag);
        Ok(tag)
      }
    }.recoverTotal{
      e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
    }
  }
  
}