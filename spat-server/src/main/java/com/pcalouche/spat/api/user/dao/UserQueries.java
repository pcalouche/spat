package com.pcalouche.spat.api.user.dao;

public class UserQueries {
    public static final String GET_BY_USERNAME = "SELECT * FROM users WHERE username=:username";
    public static final String GET_USERS = "SELECT * FROM users ORDER BY id";
    public static final String GET_BY_ID = "SELECT * FROM users WHERE id=:id";
    public static final String INSERT_USER = "INSERT INTO users(firstName, lastName) VALUES (:firstName, :lastName)";
    public static final String UPDATE_USER = "UPDATE users SET firstName=:firstName, lastName=:lastName WHERE id=:id";
    public static final String DELETE_USER = "DELETE FROM users WHERE id=:id";
}
