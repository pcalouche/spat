package com.pcalouche.spat.service.team;

import com.pcalouche.spat.model.Team;

import java.util.List;

public interface TeamService {
    List<Team> getTeams();

    Team saveTeam(Team team);

    Boolean deleteTeam(Long id);
}
