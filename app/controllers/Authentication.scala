package controllers

import play.api._
import play.api.mvc._
import models._
import play.api.data._
import play.api.data.Forms._
import java.net.URLDecoder._
import java.net.URL

object Authentication extends Controller {

  val loginForm = Form(
    tuple(
      "username" -> text,
      "password" -> text
    )
  )  
  
  def login = Action { implicit request =>
    val (username, password) = loginForm.bindFromRequest.get
    User.login(username, password) match {
      case None => {
        val header = request.headers.get("referer")
        Logger.info(header.head)
        Redirect(new URL(header.head).getFile()).flashing("error" -> "Invalid credentials")
      }
      case Some(u) => {
        val header = request.headers.get("referer")
        Logger.info(header.head)
        Redirect(new URL(header.head).getFile()).withSession(Security.username -> u.name)
      }
    }
  }
  
  def logout = Action { implicit request =>
    val header = request.headers.get("referer")
    Logger.info(header.head)
    Redirect(new URL(header.head).getFile()).withNewSession.flashing("success" -> "You are now logged out.")
  }
}

trait Secured {

  def username(request: RequestHeader) = request.session.get(Security.username)

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.wiki("")).flashing("error" -> "Not authorised")

  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }

  /**
   * This method shows how you could wrap the withAuth method to also fetch your user
   */
  def withUser(f: User => Request[AnyContent] => Result) = withAuth { username => implicit request =>
    User.getByUsername(username) match { 
      case Some(user) => f(user)(request)
      case None => onUnauthorized(request)
    }
  }

}
