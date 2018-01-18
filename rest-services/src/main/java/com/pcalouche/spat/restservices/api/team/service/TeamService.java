package com.pcalouche.spat.restservices.api.team.service;

import com.pcalouche.spat.restservices.api.model.Team;

import java.util.List;

public interface TeamService {
    List<Team> getTeams();

    Team saveTeam(Team team);

    Boolean deleteTeam(Long id);
}
