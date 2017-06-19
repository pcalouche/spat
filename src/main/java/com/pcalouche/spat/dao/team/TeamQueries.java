package com.pcalouche.spat.dao.team;

public class TeamQueries {
    public static final String GET_TEAMS = "SELECT * FROM teams ORDER BY id";
    public static final String GET_BY_ID = "SELECT * FROM teams WHERE id=:id";
    public static final String INSERT_TEAM = "INSERT INTO teams(name, sport, league) VALUES (:name, :sport, :league)";
    public static final String UPDATE_TEAM = "UPDATE teams SET name=:name, sport=:sport, league=:league WHERE id=:id";
    public static final String DELETE_TEAM = "DELETE FROM teams WHERE id=:id";
}
