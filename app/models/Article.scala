package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import java.sql.SQLException
import play.api.Logger
import scala.util.parsing.combinator._
import controllers.routes
import java.sql.Timestamp

case class ArticleNotFoundException(smth:String)  extends Exception
case class RestrictedWordException(smth:String)  extends Exception
case class RenameCollisionException(smth:String)  extends Exception

class Article(val id: Long, val title: String, val content: String, val last_edit: Double = 0) {
  
  val wikiTitle = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "-_").toSet
  val wikiMarkup = "*-/_:" toSet
  val lineBreak = sys.props("line.separator")

  val editted = last_edit match{
    case 0 => new Timestamp(System.currentTimeMillis).toString.takeWhile(x => x != '.')
    case t => new Timestamp(t.toLong * 1000).toString.takeWhile(x => x != '.')
  }
  
  /**
   * @see parse(toGo, past, current, matching)
   */
  private def parse(toGo: List[Char]): (String, String) = {
    parse(toGo, List(), List(), None)
  }
  
  /**
   * @param toGo - the list of characters still to be parsed
   * @param past - list of characters completely parsed
   * @param current - list of characters parsed, but in use (eg midway through a span tag)
   * @param matching - char indicating what the current list is wrapped in
   * 
   * @return (String, String) - Tuple containing the parsed line, and the type, eg p, ul, or nothing)
   */
  private def parse(toGo: List[Char], past: List[Char], current: List[Char], matching: Option[Char]) : (String, String) = {
    // There has to be a better way to do this....
    (matching, toGo) match {
      // Match whole line logic first
      case (Some('$'), t) if t.startsWith(List(' ', ' ', ' ')) => {
        t.dropWhile(x => x == ' ') match {
          case ('*' :: t) => ("<li>" + parse(t)._1 + "</li>", "ul")  
          case ('#' :: t) => ("<li>" + parse(t)._1 + "</li>", "ol")
          case ('h' :: '1' :: t) => ("<h1>" + parse(t)._1 + "</h1>", "")
          case ('h' :: '2' :: t) => ("<h2>" + parse(t)._1 + "</h2>", "")
          case ('h' :: '3' :: t) => ("<h3>" + parse(t)._1 + "</h3>", "")
          case ('h' :: '4' :: t) => ("<h4>" + parse(t)._1 + "</h4>", "")
          case ('h' :: '5' :: t) => ("<h5>" + parse(t)._1 + "</h5>", "")
          case ('h' :: '6' :: t) => ("<h6>" + parse(t)._1 + "</h6>", "")
          case ('~' :: t) => (t.mkString.replaceAll(" ", "&nbsp;") + "\n", "pre")
          case _ => parse(t)
        }
      }
      case (Some('$'), '|' :: t) => {
        val cols = t.foldLeft(List(List.empty[Char])) {
          (acc, i) =>
            if (i == '|') acc :+ List.empty
            else acc.init :+ (acc.last :+ i)
        }
        val parsedCols = for (s <- cols) yield {
          "<td>" + parse(s, List(), List(), Some('$'))._1 + "</td>"
        }
        ("<tr>" + parsedCols.dropRight(1).mkString + "</tr>", "table")
      }
      case (Some('$'), t) => {
        if (t == List('-', '-')) {
          ("<hr/>", "") 
        } else {
          parse(toGo)
        }
      }
      case (Some(':'), ':' :: t) => {
        val foundArticle = Article.getArticleByName(current.reverse.mkString)
        val title = t.takeWhile(x => wikiTitle(x))
        val link:String = foundArticle.id match {
          case -1 => "<a href=\"" + controllers.routes.Application.wikiEdit(foundArticle.title).url + "\" class=\"missing\"> " + title.mkString + "</a>"
          case _ => "<a href=\"" + controllers.routes.Application.wiki(foundArticle.title).url + "\"> " + title.mkString + "</a>"
        }
        parse(t.dropWhile(x => wikiTitle(x)), link.toList.reverse ::: past, List(), None)
      }
      case (Some(':'), c :: t) if (wikiTitle.apply(c) == false) => {
        val foundArticle = Article.getArticleByName(current.reverse.mkString)
        val link:String = foundArticle.id match {
          case -1 => "<a href=\"" + controllers.routes.Application.wikiEdit(foundArticle.title).url + "\" class=\"missing\"> " + foundArticle.title + "</a>" + c
          case _ => "<a href=\"" + controllers.routes.Application.wiki(foundArticle.title).url +"\"> " + foundArticle.title + "</a>" + c
        }
        parse(t, link.toList.reverse ::: past, List(), None)
      }
      case (Some(':'), Nil) => {
        val foundArticle = Article.getArticleByName(current.reverse.mkString)
        foundArticle.id match {
          case -1 => ("<a href=\"" + controllers.routes.Application.wikiEdit(foundArticle.title).url + "\" class=\"missing\"> " + foundArticle.title + "</a>", "p")
          case _ => ("<a href=\"" + controllers.routes.Application.wiki(foundArticle.title).url +"\"> " + foundArticle.title + "</a>", "p")
        }
      }
      case (Some(x), y :: t) if (x == y) => {
        // Should check that we're at the end of a word, not a hyphen, expression etc
        t match {
          case Nil | ' ' :: _ => {
            val inner = parse(current.reverse)._1
            // Sort out the wiki char mappings here
            val wrapped = x match {
              case '*' => "<span style=\"font-weight:bold;\">" + inner + "</span>"
              case '/' => "<span style=\"font-style:italic\">" + inner + "</span>"
              case '_' => "<span style=\"text-decoration:underline\">" + inner + "</span>"
              case '-' => "<span style=\"text-decoration:line-through\">" + inner + "</span>"
              case _ => inner
            }
            parse(t, wrapped.toList.reverse ::: past, List(), None)
          }  
          case _ => parse(t, past, y :: current, Some(x))
        }
      }
      case (Some(x), y :: t) => parse(t, past, y :: current, Some(x))
      case (Some(x), Nil) => parse(current.reverse, x :: past, List(), None)
      case (None, h :: t) => {
        if (wikiMarkup(h) && (current.isEmpty || current.head == ' ')) {
          parse(t, current ::: past, List(), Some(h))
        } else {
          parse(t, past, h :: current, None)
        }
      } 
      case (None, Nil) => ((past.reverse ::: current.reverse) mkString, "p")
    }
  }
  
  private def parsedStringBuilder(parsedInfo: Iterator[(String, String)]): String = {
    var last = ""
    (for (line <- parsedInfo) yield (
      (line, last) match {
        case ((s, tag), l) if tag == l && tag == "p" => last = tag; s + "<br/>"
        case ((s, tag), l) if tag == l => last = tag; s
        case ((s, tag), l) if tag == "" => last = tag; "</" + last + ">" + s
        case ((s, tag), l) if l == "" && tag == "p" => last = tag; "<" + tag + ">" + s + "<br/>"
        case ((s, tag), l) if l == "" => last = tag; "<" + tag + ">" + s
        case ((s, tag), l) if tag == "p" => last = tag; "</" + l + "><" + tag + ">" + s + "<br/>"
        case ((s, tag), l) => last = tag; "</" + l + "><" + tag + ">" + s
      }
    )).mkString
  } 
  
  def toHTML: String = {
    parsedStringBuilder(for (l <- content.lines) yield parse(l.toList, List(), List(), Some('$')))
  }

  def firstParagraph: String = {
    parsedStringBuilder(for (l <- content.lines.takeWhile(x => x != "")) yield parse(l.toList, List(), List(), Some('$')))
  }
    
  override def toString: String = {
    content
  }
}

object Article {
  val parse = {
    get[Long]("id") ~
    get[String]("title") ~ 
    get[String]("content") ~
    get[Double]("last_edit") map {
      case i~n~c~l => new Article(i, n, c, l)
    }
  }

  val reservedPageWords = List("New", "Unknown", "", "Recent").toSet[String]
    
  def save(name: String, content: String) {
    val sensibleName = name.head.toUpper + name.tail
    if (reservedPageWords(sensibleName)) {
      throw throw RestrictedWordException("Can't create or rename a page to " + sensibleName)
    }
    DB.withConnection{implicit c =>
      SQL("""
        select count(*) from tArticle where title = {title}
      """).on('title -> sensibleName).as(scalar[Long].single) match {
        case 0 => {
          DB.withConnection{implicit c =>
            SQL("""
              insert into tArticle (title, content) values ({title}, {content})
            """).on('title -> sensibleName, 'content -> content).executeUpdate()

          }
        }
        case _ => throw RenameCollisionException("Title " + sensibleName + " already exists - Use edit instead")
      }
    }
  }
  def save(id: Long, name: String, content: String) {
    if (reservedPageWords(name)) {
      throw RestrictedWordException("Can't create or rename a page to " + name)
    }
    DB.withConnection{implicit c =>
      SQL("""
        update tArticle set title = {title}, content = {content}, last_edit = now() where id = {id}
      """).on('title -> name, 'content -> content, 'id -> id).executeUpdate()
    }
  }
  
  def getAll : List[Article] = {
    DB.withConnection{implicit c =>
      SQL("""
        select id, title, content, EXTRACT(EPOCH FROM last_edit) last_edit from tArticle order by last_edit DESC
      """).as(Article.parse*)
    }
  }
  
  def getArticleByName(name: String): Article = {
    if (reservedPageWords(name)) {
      name match {
        case "" => getArticleByName("Home") 
        case "New" => new Article(-1, "", "") 
        case "Unknown" => new Article(-1, "Not Found", "Page Not Found") 
      }
    } else {
      DB.withConnection{ implicit c =>
        SQL("""
          select id, title, content, EXTRACT(EPOCH FROM last_edit) last_edit from tArticle where title = {name}
        """).on('name -> name).as(Article.parse.singleOpt) match {
          case Some(x) => x 
          case _ => new Article(-1, name, "")
        }
      }
    }
  }

}