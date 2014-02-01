package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import java.sql.SQLException
import play.api.Logger
import scala.util.parsing.combinator._
import controllers.routes

case class Tag(id: Long, name: String)

object Tag {
  
  val parse = {
    get[Long]("id") ~
    get[String]("tag") map {
      case i~n => new Tag(i, n)
    }
  }

  def getAllTags(locked: Boolean): List[(Tag, Long)] = {
    val articleLock = if (locked) "%" else "U"
    DB.withConnection(implicit c =>
      SQL("""
        select
          t.id,
          t.tag,
          (select count(*) from tArticleTag ta inner join tArticle a on (a.id=ta.article_id) where a.locked like {locked} ta.tag_id = t.id) count 
        from
          tTag t
      """).on('locked -> articleLock).as(Tag.parse ~ long("count") map(flatten) *)
    )
  }
  
  def getTagsForArticle(id: Long): List[Tag] = {
    DB.withConnection(implicit c =>
      SQL("""
        select
          t.id,
          t.tag
        from
          tArticleTag ta
          inner join tTag t
            on (t.id = ta.tag_id)
        where ta.article_id = {id}
        order by t.tag ASC
      """).on('id -> id).as(Tag.parse.*)
    )
  }
  
  def getArticlesWithTag(tag: String, locked: Boolean): List[Article] = {
    val articleLocked = if (locked) "%" else "U"
    DB.withConnection(implicit c =>
      SQL("""
        select
          a.id, a.title, a.content, EXTRACT(EPOCH FROM a.last_edit) last_edit
        from
          tArticle a
          inner join tArticleTag at on (at.article_id = a.id)
          inner join tTag t on (at.tag_id = t.id) 
        where 
          t.tag = {tag}
          a.locked like {locked}
        order by last_edit DESC
      """).on('tag -> tag, 'locked -> articleLocked).as(Article.parse *)
    )
  }
  
  /**
   * Gets a tag by any means necessary - if it doesn't exist, create it
   * 
   * @param String tag
   *   name of the tag to get
   *   
   * @returns Tag
   */
  def getTag(tag: String, make: Boolean = false): Option[Tag] = {
    DB.withConnection(implicit c =>
      SQL("""
        select id, tag from tTag where tag = {tag}
      """).on('tag -> tag).as(Tag.parse.singleOpt)
    ) match {
      case None => {
        if (make) {
          DB.withConnection(implicit c =>
            SQL("""
              insert into tTag (tag) values ({name})
            """).on('name -> tag).executeUpdate
          )
          getTag(tag)
        } else {
          None
        }
      }
      case Some(t) => Some(t)
    }
  }
    
  /**
   * Adds a tag to an article
   * 
   * @param Long articleId
   *   id of the article
   * @param String tag
   *   tag to add
   */
  def addTagToArticle(articleId: Long, tag: String) {
    //get tag (create if needed)
    val t: Tag = getTag(tag, true) match {
      case Some(t) => t
      case _ => throw new Exception()
    }
    
    //add entry to tArticleTag
    DB.withConnection(implicit c =>
      SQL("""
        insert into tArticleTag (article_id, tag_id) values ({article}, {tag})
      """).on('article -> articleId, 'tag -> t.id).executeUpdate
    )
  }

  /**
   * Removes a tag from an article
   * 
   * @param Long articleId
   *   id of the article
   * @param String tag
   *   tag to remove
   */
  def removeTagFromArticle(articleId: Long, tag: String) {
    //get tag (but don't create it, if it doesn't exist do nothing)
    getTag(tag) match {
      case Some(t) => { 
        //add entry to tArticleTag
        DB.withConnection(implicit c =>
          SQL("""
            delete from tArticleTag where article_id = {article} and tag_id = {tag}
          """).on('article -> articleId, 'tag -> t.id).executeUpdate
        )
      }
      case _ => { }
    }
  }
  
  def deleteTag(tag: String): Boolean = {
    getArticlesWithTag(tag, true).size match {
      case 0 => {
        DB.withConnection(implicit c =>
          SQL("""
            delete from tTag where tag = {tag}
          """).on('tag -> tag).executeUpdate
        )
        true
      }
      case _ => false
    }
  }
  
}