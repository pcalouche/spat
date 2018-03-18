INSERT INTO teams (name) VALUES ('Team Dev Pool');
INSERT INTO teams (name) VALUES ('Team Kepler');
INSERT INTO teams (name) VALUES ('Team Green');
INSERT INTO teams (name) VALUES ('Musketeers');
INSERT INTO teams (name) VALUES ('Tech-Knights');
INSERT INTO teams (name) VALUES ('Team Ninja');
INSERT INTO teams (name) VALUES ('Team Heimdall');

INSERT INTO users (username, password, account_non_expired, credentials_non_expired, account_non_locked, enabled, authorities)
VALUES ('activeUser', '$2a$10$fjqJYwzpTxqpIus8PvwFpe2/qlK2yjU0n.uQkrZKXR1lOGSWPVQM2', TRUE, TRUE, TRUE, TRUE, 'ROLE_USER');
INSERT INTO users (username, password, account_non_expired, credentials_non_expired, account_non_locked, enabled, authorities)
VALUES ('activeAdmin', '$2a$10$fjqJYwzpTxqpIus8PvwFpe2/qlK2yjU0n.uQkrZKXR1lOGSWPVQM2', TRUE, TRUE, TRUE, TRUE, 'ROLE_USER, ROLE_ADMIN');
INSERT INTO users (username, password, account_non_expired, credentials_non_expired, account_non_locked, enabled, authorities)
VALUES ('expiredUser', '$2a$10$fjqJYwzpTxqpIus8PvwFpe2/qlK2yjU0n.uQkrZKXR1lOGSWPVQM2', FALSE, TRUE, TRUE, TRUE, 'ROLE_USER');
INSERT INTO users (username, password, account_non_expired, credentials_non_expired, account_non_locked, enabled, authorities)
VALUES ('credentialsExpiredUser', '$2a$10$fjqJYwzpTxqpIus8PvwFpe2/qlK2yjU0n.uQkrZKXR1lOGSWPVQM2', TRUE, FALSE, TRUE, TRUE, 'ROLE_USER');
INSERT INTO users (username, password, account_non_expired, credentials_non_expired, account_non_locked, enabled, authorities)
VALUES ('lockedUser', '$2a$10$fjqJYwzpTxqpIus8PvwFpe2/qlK2yjU0n.uQkrZKXR1lOGSWPVQM2', TRUE, TRUE, FALSE, TRUE, 'ROLE_USER');
INSERT INTO users (username, password, account_non_expired, credentials_non_expired, account_non_locked, enabled, authorities)
VALUES ('disabledUser', '$2a$10$fjqJYwzpTxqpIus8PvwFpe2/qlK2yjU0n.uQkrZKXR1lOGSWPVQM2', TRUE, TRUE, TRUE, FALSE, 'ROLE_USER');