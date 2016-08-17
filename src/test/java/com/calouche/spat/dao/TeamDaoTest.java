package com.calouche.spat.dao;

import com.calouche.spat.IntegratedTest;
import com.calouche.spat.dao.team.TeamDao;
import com.calouche.spat.model.Team;
import org.springframework.beans.factory.annotation.Autowired;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class TeamDaoTest extends IntegratedTest {
    @Autowired
    TeamDao teamDao;

    @Test
    void getTeamsTest() {
        List<Team> teams = teamDao.getTeams();
        Assert.assertNotNull(teams);
    }

    @Test
    void saveTeamTest() {
        Team newTeam = new Team(null, "Team1", "Basketball", "NBA");

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
