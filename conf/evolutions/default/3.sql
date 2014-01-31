# --- !Ups

CREATE TABLE tUser(
    id           serial        primary key,
    username     varchar(64)   NOT NULL,
    password     varchar(64)   NOT NULL,
    salt         varchar(16)   NOT NULL,
    email        varchar(255)  NOT NULL
);

CREATE UNIQUE INDEX iUser1 ON tUser (username);
CREATE UNIQUE INDEX iUser2 ON tUser (email);

# --- !Downs

DROP TABLE tUser;
