package com.pcalouche.spat.service.team;

import com.pcalouche.spat.dao.team.TeamDao;
import com.pcalouche.spat.model.Team;
import com.pcalouche.spat.service.AbstractServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class TeamServiceImpl extends AbstractServiceImpl implements TeamService {
    private final TeamDao teamDao;

    @Autowired
    public TeamServiceImpl(TeamDao teamDao) {
        this.teamDao = teamDao;
    }

    @Override
    public List<Team> getTeams() {
        return teamDao.getTeams();
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
