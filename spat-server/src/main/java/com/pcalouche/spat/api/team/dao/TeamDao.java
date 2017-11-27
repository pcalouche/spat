package com.pcalouche.spat.api.team.dao;

import com.pcalouche.spat.api.model.Team;

import java.util.List;

public interface TeamDao {
    List<Team> getTeams();

    Team saveTeam(Team team);

    Boolean deleteTeam(Long id);
}
