import play.api._
import models._
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._

object Global extends GlobalSettings {
  override def onStart(app: Application) {
    InitialData.insert()
  }  
}

/**
 * Initial wiki articles to be imported 
 */
object InitialData {
  def insert() = {
    Seq(
      ("Home", """   +++Welcome to CaffeineWiki

Create your own homepage: Check out the :Formatting rules, and go wild"""),
      ("Formatting", """|*Markup*|*Appears*|*Description*|
|   ~*Text*|*Text*|Bold|
|   ~_Text_|_Text_|Underlined|
|   ~-Text-|-Text-|Strikethrough|
|   ~:Article|:Article|Link|
|   ~:Article:Name|:Article:Name|Renamed Link|
|   ~   +HEADER|   +HEADER|h1 header|
|   ~   ++HEADER|   ++HEADER|h2 header|
|   ~   +++HEADER|   +++HEADER|h3 header|
|   ~   ++++HEADER|   ++++HEADER|h4 header|
|   ~   +++++HEADER|   +++++HEADER|h5 header|
|   ~   ++++++HEADER|   ++++++HEADER|h6 header|
|   ~   ~Text|~Text~|Text without markup|
|   ~--|--|Horizontal Rule|

--

   ~   * Unordered Bullet Points
   * One
   * Two
   * Three

--

   ~   # Ordered Bullet Points
   # One
   # Two
   # Three

--

   ~|*Col1*|*Col2*|*Col3*|
   ~|1,1|1,2|1,3|
   ~|2,1|2,2|2,3|

|*Col1*|*Col2*|*Col3*|
|1,1|1,2|1,3|
|2,1|2,2|2,3|""")
    ).foreach(x => {
      if (Article.getArticleByName(x._1).id == -1) {
        Logger.info("Article not found: Creating default for " + x._1)
        Article.save(x._1, x._2)
      }
    })
    
    Seq("Main").foreach(x => {Tag.getTag(x)})

    // Add my own user - yeah its in github, but if you want to create the whole rainbow table to figure out my password go ahead....
    User.getByUsername("caffeinateddave") match {
      case None => DB.withConnection {implicit c =>
        SQL("""
          insert into tUser (username, password, salt, email) 
            values ({name}, {pass}, {salt}, {email})
        """)
        .on('name -> "caffeinateddave", 'pass -> "91FCE24019FDC65EC763922797F476E1", 'salt -> "vwykJvOsSQaiXHXz", 'email -> "dave@caffeinateddave.com")
        .executeUpdate
      }
      case Some(x) => {}
    }

  }
}

