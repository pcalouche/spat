package com.pcalouche.spat.api.team.service;

import com.pcalouche.spat.api.model.Team;

import java.util.List;

public interface TeamService {
    List<Team> getTeams();

    Team saveTeam(Team team);

    Boolean deleteTeam(Long id);
}
