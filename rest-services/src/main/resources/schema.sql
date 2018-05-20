DROP TABLE IF EXISTS teams;
DROP TABLE IF EXISTS users;

CREATE TABLE teams (
  id   IDENTITY PRIMARY KEY,
  name VARCHAR(150) DEFAULT NULL
);

CREATE TABLE users (
  id                      IDENTITY PRIMARY KEY,
  username                VARCHAR(50)  NOT NULL,
  password                VARCHAR(200) NOT NULL,
  account_non_expired     BOOLEAN      DEFAULT TRUE,
  credentials_non_expired BOOLEAN      DEFAULT TRUE,
  account_non_locked      BOOLEAN      DEFAULT TRUE,
  enabled                 BOOLEAN      DEFAULT TRUE,
  authorities             VARCHAR(200) DEFAULT 'USER'
);