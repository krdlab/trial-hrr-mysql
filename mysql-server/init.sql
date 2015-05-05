USE test;

DROP TABLE IF EXISTS tagging;
DROP TABLE IF EXISTS tag;
DROP TABLE IF EXISTS `comment`;
DROP TABLE IF EXISTS blog_entry;
DROP TABLE IF EXISTS blog;
DROP TABLE IF EXISTS user;

CREATE TABLE user (
  id    INT         NOT NULL,
  name  VARCHAR(64) NOT NULL,
  PRIMARY KEY(id)
) ENGINE=InnoDB;

CREATE TABLE blog (
  id      INT           NOT NULL,
  title   VARCHAR(128)  NOT NULL,
  user_id INT           NOT NULL,
  PRIMARY KEY (id),
  INDEX fk_blog_user_idx (user_id ASC),
  CONSTRAINT fk_blog_user FOREIGN KEY (user_id) REFERENCES user (id)
) ENGINE = InnoDB;

CREATE TABLE blog_entry (
  id          INT           NOT NULL,
  title       VARCHAR(128)  NOT NULL,
  content     TEXT          NOT NULL,
  created_at  DATETIME      NOT NULL,
  blog_id     INT           NOT NULL,
  PRIMARY KEY (id),
  INDEX fk_blog_entry_blog_idx (blog_id ASC),
  CONSTRAINT fk_blog_entry_blog FOREIGN KEY (blog_id) REFERENCES blog (id)
) ENGINE = InnoDB;

CREATE TABLE `comment` (
  id            INT           NOT NULL,
  body          VARCHAR(300)  NOT NULL,
  created_at    DATETIME      NOT NULL,
  blog_entry_id INT           NOT NULL,
  user_id       INT           NOT NULL,
  PRIMARY KEY (`id`),
  INDEX fk_comment_blog_entry_idx (blog_entry_id ASC),
  INDEX fk_comment_user_idx       (user_id ASC),
  CONSTRAINT fk_comment_blog_entry  FOREIGN KEY (blog_entry_id) REFERENCES blog_entry (id),
  CONSTRAINT fk_comment_user        FOREIGN KEY (user_id)       REFERENCES user (id)
) ENGINE = InnoDB;

CREATE TABLE tag (
  id    INT         NOT NULL,
  value VARCHAR(32) NOT NULL,
  PRIMARY KEY (id)
) ENGINE = InnoDB;

CREATE TABLE tagging (
  blog_entry_id INT NOT NULL,
  tag_id        INT NOT NULL,
  PRIMARY KEY (blog_entry_id, tag_id),
  INDEX fk_tagging_blog_entry_idx (blog_entry_id ASC),
  INDEX fk_tagging_tag_idx        (tag_id ASC),
  CONSTRAINT fk_tagging_blog_entry  FOREIGN KEY (blog_entry_id) REFERENCES blog_entry (id),
  CONSTRAINT fk_tagging_tag         FOREIGN KEY (tag_id)        REFERENCES tag (id)
) ENGINE = InnoDB;

START TRANSACTION;
SET autocommit=0;

INSERT INTO user
    (id, name)
  VALUES
      (1, 'user1')
    , (2, 'user2')
    , (3, 'user3')
;
INSERT INTO blog
    (id, title, user_id)
  VALUES
      (101, 'user1\'s blog', 1)
    , (102, 'user2\'s blog', 2)
    , (103, 'user3\'s blog', 3)
;
INSERT INTO blog_entry
    (id, title, content, created_at, blog_id)
  VALUES
      (10101, 'user1\'s entry1', 'content 10101', '2015-05-01 09:00:00', 101)
    , (10102, 'user1\'s entry2', 'content 10102', '2015-05-02 09:00:00', 101)
    , (10201, 'user2\'s entry1', 'content 10201', '2015-05-02 13:00:00', 102)
;
INSERT INTO tag
    (id, value)
  VALUES
      (1, 'tag1')
    , (2, 'tag2')
    , (3, 'tag3')
;
INSERT INTO tagging
    (blog_entry_id, tag_id)
  VALUES
      (10101, 1)
    , (10201, 2)
    , (10201, 3)
;
INSERT INTO `comment`
    (id, body, created_at, blog_entry_id, user_id)
  VALUES
      (1010103, 'user3\'s comment to user1\'s entry1', '2015-05-01 10:00:00', 10101, 3)
    , (1010102, 'user2\'s comment to user1\'s entry1', '2015-05-01 10:30:00', 10101, 2)
;

COMMIT;
