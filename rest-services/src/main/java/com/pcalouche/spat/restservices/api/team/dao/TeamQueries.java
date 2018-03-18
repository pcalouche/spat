package com.pcalouche.spat.restservices.api.team.dao;

public class TeamQueries {
    public static final String GET_TEAMS = "SELECT * FROM teams ORDER BY id";
    public static final String GET_BY_ID = "SELECT * FROM teams WHERE id=:id";
    public static final String INSERT_TEAM = "INSERT INTO teams(name) VALUES (:name)";
    public static final String UPDATE_TEAM = "UPDATE teams SET name=:name WHERE id=:id";
    public static final String DELETE_TEAM = "DELETE FROM teams WHERE id=:id";
}
