# --- !Ups

CREATE TABLE tArticle(
    id        serial        primary key,
    title     varchar(128)  NOT NULL,
    content   text          NOT NULL default '',
    last_edit timestamp     NOT NULL DEFAULT now()
);

CREATE INDEX iArticle1 ON tArticle (title);

insert into tArticle (title, content) values ('Home', 'Dummy homepage data! Go Dave!');

# --- !Downs

DROP TABLE tArticle;
