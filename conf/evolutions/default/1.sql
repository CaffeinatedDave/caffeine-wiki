# --- !Ups

CREATE TABLE tArticle(
    id        serial        primary key,
    title     varchar(128)  NOT NULL,
    content   text          NOT NULL default '',
    last_edit timestamp     NOT NULL DEFAULT now()
);

CREATE INDEX iArticle1 ON tArticle (title);

insert into tArticle (title, content) values 
('Home', '   h3Welcome to CaffeineWiki.

There isn''t much here yet, but you can edit this page to your own homepage above, or create new pages. Take a look at the :Formatting page for the available markup.

Since its still a personal project, I might as well have my own links here too:

|*Page*|*Description*|
|:ToDo|The list of things I want to add to this, in no particular order|
|:Sandbox|Sandbox for new things|'),
('Formatting', '|*Markup*|*Appears*|
|~*Bold*~|*bold*|
|~_underlined_~|_underlined_|
|~-strikethrough-~|-strikethrough-|
|~:Home~|:Home|
|~:Home:RenamedHome~|:Home:RenamedHome|
|~   h1HEADER~|   h1HEADER|
|~   h2HEADER~|   h2HEADER|
|~   h3HEADER~|   h3HEADER|
|~   h4HEADER~|   h4HEADER|
|~   h5HEADER~|   h5HEADER|
|~   h6HEADER~|   h6HEADER|
|~preformatted~|~preformatted~|

   * One
   * Two
   * Three
   * Four

~Some *wiki* _markup_ -except- not.~

   # One
   # Two
   # Three
   # Four

|*Col1*|*Col2*|*Col3*|
|1,1|1,2|1,3|
|2,1|2,2|2,3|'),
('Sandbox', 'Use this page to test things, without ruining other pages'),
('ToDo', '|*Feature*|*Complexity*|*Description*|
|Images|High|Need to be able to add images, this will involve uploading the image, and being able to browse them to add them to a page|
|More markup|Low-Medium|Would be useful to have more markup - Things like table design (multicolumn etc) and multiline formatting|
|Better CSS|Low|Adding responsive design will be useful for when we have images|
|WYSIWYG Editor|High|Ability to see the changes you''re making in an editor as you type|
|Export|Low|Will be able to take wiki pages from one DB and add/meld them into another (already have script)|
|List of pages|Low|Would be nice to see at-a-glance which pages are in the system|
|Changing titles fixes links in articles|Medium|If the link changes currently, articles will point to the wrong place. Think of some way to change this|
|Delete pages|Low|Need to remove dead pages - will involve page lists, perhaps an orphan list too?|
|Last edit|Low|Just a timestamp on articles to say when they''ve been changed|
|History|High|A diff of revisions between changes, or at least old versions|
|Users|Medium|Add who changed what, and perhaps limit changes to registered users|');


# --- !Downs

DROP TABLE tArticle;
