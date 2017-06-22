package com.pcalouche.spat.dao;

import com.pcalouche.spat.IntegratedTest;
import com.pcalouche.spat.dao.team.TeamDao;
import com.pcalouche.spat.model.Team;
import org.springframework.beans.factory.annotation.Autowired;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class TeamDaoTest extends IntegratedTest {
    @Autowired
    private TeamDao teamDao;

    @Test
    void getTeamsTest() {
        List<Team> teams = teamDao.getTeams();
        Assert.assertNotNull(teams);
    }

    @Test
    void saveTeamTest() {
        Team newTeam = new Team(null, "Team1");

        Team savedTeam = teamDao.saveTeam(newTeam);
        Assert.assertEquals(savedTeam, newTeam);

        savedTeam.setName("NewName");

        Team updatedTeam = teamDao.saveTeam(savedTeam);
        Assert.assertEquals(updatedTeam, savedTeam);
    }

    @Test
    void deleteTeamTest() {
        Assert.assertTrue(teamDao.deleteTeam(1L));
        Assert.assertFalse(teamDao.deleteTeam(-1L));
    }
}
