# --- !Ups

ALTER TABLE tArticle ADD COLUMN locked char(1) NOT NULL DEFAULT 'U';

# --- !Downs

ALTER TABLE tArticle DROP COLUMN IF EXISTS locked;