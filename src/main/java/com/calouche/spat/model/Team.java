package com.calouche.spat.model;

public class Team {
    public String league;
    public String sport;
    private Long id;
    private String name;

    public Team() {
    }

    public Team(Long id, String name, String sport, String league) {
        this.id = id;
        this.name = name;
        this.sport = sport;
        this.league = league;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSport() {
        return sport;
    }

    public void setSport(String sport) {
        this.sport = sport;
    }

    public String getLeague() {
        return league;
    }

    public void setLeague(String league) {
        this.league = league;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Team team = (Team) o;

        if (league != null ? !league.equals(team.league) : team.league != null) {
            return false;
        }
        if (sport != null ? !sport.equals(team.sport) : team.sport != null) {
            return false;
        }
        return name != null ? name.equals(team.name) : team.name == null;

    }

    @Override
    public int hashCode() {
        int result = league != null ? league.hashCode() : 0;
        result = 31 * result + (sport != null ? sport.hashCode() : 0);
        result = 31 * result + (name != null ? name.hashCode() : 0);
        return result;
    }
}
