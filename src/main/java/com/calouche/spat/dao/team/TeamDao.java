package com.calouche.spat.dao.team;

import com.calouche.spat.model.Team;

import java.util.List;

public interface TeamDao {
    List<Team> getTeams();

    Team saveTeam(Team team);

    Boolean deleteTeam(Long id);
}
