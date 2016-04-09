package com.calouche.spat.service.team;

import com.calouche.spat.model.Team;

import java.util.List;

public interface TeamService {
    List<Team> getTeams();

    Team saveTeam(Team team);

    Boolean deleteTeam(Long id);
}
