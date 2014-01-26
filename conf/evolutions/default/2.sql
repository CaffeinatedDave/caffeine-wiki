# --- !Ups

CREATE TABLE tTag(
    id           serial        primary key,
    tag          varchar(64)   NOT NULL
);

CREATE INDEX iTag1 ON tTag (tag);

CREATE TABLE tArticleTag(
    id         serial primary key,
    article_id int NOT NULL references tArticle(id),
    tag_id     int NOT NULL references tTag(id)
);

CREATE UNIQUE INDEX iArticleTag1 on tArticleTag (article_id, tag_id);

# --- !Downs

DROP TABLE tArticleTag;
DROP TABLE tTag;
