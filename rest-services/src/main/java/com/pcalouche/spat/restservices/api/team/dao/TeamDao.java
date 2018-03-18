package com.pcalouche.spat.restservices.api.team.dao;

import com.pcalouche.spat.restservices.api.entity.Team;

import java.util.List;

public interface TeamDao {
    List<Team> getTeams();

    Team saveTeam(Team team);

    Boolean deleteTeam(Long id);
}
