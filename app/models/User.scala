package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import java.sql.SQLException
import play.api.Logger
import scala.util.parsing.combinator._
import java.security.MessageDigest
import scala.util.Random

case class User(id: Long, name: String, email: String)

object User {
  val parse = {
    get[Long]("id") ~
    get[String]("username") ~
    get[String]("email") map {
      case i~n~e => new User(i, n, e)
    }
  }
  
  private def genSalt(len: Int): String = {
    val chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".toList
    var count = 0
    (for (count <- 1 to len) yield {
      chars.drop(Math.floor(Math.random * chars.size).toInt).head
    } ).mkString
  }
  
  private def hash(pass: String, salt: String): String = {
    MessageDigest.getInstance("MD5").digest((salt+pass).getBytes).map("%02X".format(_)).mkString
  }
  
  def login(username: String, password: String) : Option[User] = {
    Logger.info("Logging in " + username)
    DB.withConnection { implicit c =>
      SQL(
        """ 
          select salt from tUser where username = {user}
        """).on('user -> username).as(scalar[String].singleOpt) match {
        case None => None 
        case Some(x) => {
          Logger.info("Found salt: " + x)
          Logger.debug(hash(password, x))
          SQL("""
            select * from tUser where username = {user} and password = {pass}
          """).on('user -> username, 'pass -> hash(password, x)).as(User.parse.singleOpt)
        }
      }   
    }
  }

  def getByUsername(username: String) : Option[User] = { 
    DB.withConnection { implicit c =>
      SQL("""
        select * from tUser where username = {user}
      """).on(
        'user     -> username
      ).as(User.parse.singleOpt)
    }   
  }


  def create(name: String, email: String, password: String) {
    val salt = genSalt(16)
    val pass = hash(password, salt)
    DB.withConnection { implicit c =>
      SQL("insert into tUser (username, email, password, salt) values ({name}, {email}, {password}, {salt})").on(
        'name     -> name,
        'email    -> email,
        'password -> pass,
        'salt     -> salt
      ).executeUpdate()
    }
  }
 
}