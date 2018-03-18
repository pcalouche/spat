package com.pcalouche.spat.restservices.api.user.dao;

public class UserQueries {
    public static final String GET_BY_USERNAME = "SELECT * FROM users WHERE username=:username";
    public static final String GET_USERS = "SELECT * FROM users ORDER BY id";
    public static final String GET_BY_ID = "SELECT * FROM users WHERE id=:id";
    public static final String INSERT_USER = "INSERT INTO users(username, password, account_non_expired, account_non_locked, credentials_non_expired, enabled, authorities) " +
            "VALUES (:username, :password, :accountNonExpired, :accountNonLocked, :credentialsNonExpired, :enabled, :authorities)";
    public static final String UPDATE_USER = "UPDATE users SET username=:username, password=:password, account_non_expired=:accountNonExpired, " +
            "account_non_locked=:accountNonLocked, credentials_non_expired=:credentialsNonExpired, enabled=:enabled, authorities=:authorities WHERE id=:id";
    public static final String DELETE_USER = "DELETE FROM users WHERE id=:id";
}
