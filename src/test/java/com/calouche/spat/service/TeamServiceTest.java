package com.calouche.spat.service;

import com.calouche.spat.dao.team.TeamDao;
import com.calouche.spat.model.Team;
import com.calouche.spat.service.team.TeamService;
import com.calouche.spat.service.team.TeamServiceImpl;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

public class TeamServiceTest {
    private final TeamDao teamDao = Mockito.mock(TeamDao.class);
    private final TeamService teamService = new TeamServiceImpl(teamDao);

    @Test
    public void getTeamsTest() {
        List<Team> expectedTeams = new ArrayList<>();
        expectedTeams.add(new Team(1L, "Team1", "Basketball", "NBA"));
        expectedTeams.add(new Team(2L, "Team2", "Soccer", "FIBA"));

        Mockito.when(teamDao.getTeams()).thenReturn(expectedTeams);

        List<Team> actualTeams = teamService.getTeams();
        Assert.assertEquals(actualTeams.size(), expectedTeams.size());
        for (int i = 0; i < actualTeams.size(); i++) {
            Assert.assertEquals(actualTeams.get(i), expectedTeams.get(i));
        }
    }

    @Test
    void saveTeamTest() {
        Team expectedTeam = new Team(1L, "Team1", "Basketball", "NBA");

        Mockito.when(teamDao.saveTeam(expectedTeam)).thenReturn(expectedTeam);

        Team actualTeam = teamService.saveTeam(expectedTeam);
        Assert.assertEquals(actualTeam, expectedTeam);
    }

    @Test
    void deleteTeamTest() {
        Mockito.when(teamDao.deleteTeam(1L)).thenReturn(true);

        Assert.assertTrue(teamService.deleteTeam(1L));
    }
}