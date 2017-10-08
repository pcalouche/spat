package com.pcalouche.spat.service;

import com.pcalouche.spat.AbstractServiceTest;
import com.pcalouche.spat.dao.team.TeamDao;
import com.pcalouche.spat.model.Team;
import com.pcalouche.spat.service.team.TeamService;
import com.pcalouche.spat.service.team.TeamServiceImpl;
import org.assertj.core.api.Assertions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

public class TeamServiceTest extends AbstractServiceTest {
    @MockBean
    private TeamDao teamDao;
    private TeamService teamService;

    @Before
    public void before() {
        teamService = new TeamServiceImpl(teamDao);
    }

    @Test
    public void testGetTeams() {
        List<Team> expectedTeams = new ArrayList<>();
        expectedTeams.add(new Team(1L, "Team1"));
        expectedTeams.add(new Team(2L, "Team2"));

        BDDMockito.given(teamDao.getTeams()).willReturn(expectedTeams);

        Assertions.assertThat(teamService.getTeams()).isEqualTo(expectedTeams);

        Mockito.verify(teamDao, Mockito.times(1)).getTeams();
    }

    @Test
    public void testSaveTeam() {
        Team expectedTeam = new Team(1L, "Team1");

        BDDMockito.given(teamDao.saveTeam(expectedTeam)).willReturn(expectedTeam);

        Assertions.assertThat(teamService.saveTeam(expectedTeam)).isEqualTo(expectedTeam);

        Mockito.verify(teamDao, Mockito.times(1)).saveTeam(expectedTeam);
    }

    @Test
    public void testDeleteTeam() {
        BDDMockito.given(teamDao.deleteTeam(1L)).willReturn(true);

        Assertions.assertThat(teamService.deleteTeam(1L)).isTrue();
    }

}
