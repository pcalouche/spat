DROP TABLE IF EXISTS teams;
DROP TABLE IF EXISTS users;

CREATE TABLE teams (
  id   SERIAL PRIMARY KEY,
  name VARCHAR(150) DEFAULT NULL
);

CREATE TABLE users (
  id        SERIAL PRIMARY KEY,
  firstName VARCHAR(200) DEFAULT NULL,
  lastName  VARCHAR(200) DEFAULT NULL
);

BEGIN;
INSERT INTO teams (name) VALUES ('Team Dev Pool');
INSERT INTO teams (name) VALUES ('Team Kepler');
INSERT INTO teams (name) VALUES ('Team Green');
INSERT INTO teams (name) VALUES ('Musketeers');
INSERT INTO teams (name) VALUES ('Tech-Knights');
INSERT INTO teams (name) VALUES ('Team Ninja');
INSERT INTO teams (name) VALUES ('Team Heimdall');

INSERT INTO users (firstName, lastName) VALUES ('Homer', 'Simpson');
INSERT INTO users (firstName, lastName) VALUES ('Cobra', 'Commander');
INSERT INTO users (firstName, lastName) VALUES ('Optimus', 'Prime');
INSERT INTO users (firstName, lastName) VALUES ('Peter', 'Griffin');
INSERT INTO users (firstName, lastName) VALUES ('Luke', 'Skywalker');
COMMIT;
