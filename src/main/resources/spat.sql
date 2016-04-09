DROP TABLE IF EXISTS teams;
DROP TABLE IF EXISTS users;

CREATE TABLE teams (
    id SERIAL PRIMARY KEY,
    name VARCHAR(150) DEFAULT NULL,
    sport VARCHAR(150) DEFAULT NULL,
    league VARCHAR(150) DEFAULT NULL
);

CREATE TABLE users (
   id SERIAL PRIMARY KEY,
   firstName VARCHAR(200) DEFAULT NULL,
   lastName VARCHAR(200) DEFAULT NULL
);

BEGIN;
INSERT INTO teams (name, sport, league) VALUES('NC State Wolfpack', 'Basketball', 'NCAA');
INSERT INTO teams (name, sport, league) VALUES('NC State Wolfpack', 'Football', 'NCAA');
INSERT INTO teams (name, sport, league) VALUES('Charlote Hornets', 'Basketball', 'NBA');
INSERT INTO teams (name, sport, league) VALUES('Carolina Panthers', 'Football', 'NFL');

INSERT INTO users(firstName, lastName) VALUES('Philip','Calouche');
INSERT INTO users(firstName, lastName) VALUES('Bog','Jordan');
INSERT INTO users(firstName, lastName) VALUES('Albert','Hanes');
COMMIT;