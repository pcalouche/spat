package com.pcalouche.spat.restservices.api.team.service;

import com.pcalouche.spat.restservices.api.AbstractSpatServiceImpl;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.team.dao.TeamDao;
import com.pcalouche.spat.restservices.api.team.repository.TeamRepository;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class TeamServiceImpl extends AbstractSpatServiceImpl implements TeamService {
    private final TeamDao teamDao;
    private final TeamRepository teamRepository;

    public TeamServiceImpl(TeamDao teamDao, TeamRepository teamRepository) {
        this.teamDao = teamDao;
        this.teamRepository = teamRepository;
    }

    @Override
    public List<Team> getTeams() {
        List<Team> teams = new ArrayList<>();
        teamRepository.findAll().forEach(teams::add);
        return teams;
    }

    @Override
    public Team saveTeam(Team team) {
        return teamDao.saveTeam(team);
    }

    @Override
    public Boolean deleteTeam(Long id) {
        return teamDao.deleteTeam(id);
    }
}
