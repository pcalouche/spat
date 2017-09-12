package com.pcalouche.spat.service;

import com.pcalouche.spat.AbstractServiceTest;
import com.pcalouche.spat.dao.team.TeamDao;
import com.pcalouche.spat.model.Team;
import com.pcalouche.spat.service.team.TeamService;
import com.pcalouche.spat.service.team.TeamServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

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

        given(teamDao.getTeams()).willReturn(expectedTeams);

        assertThat(teamService.getTeams()).isEqualTo(expectedTeams);

        verify(teamDao, times(1)).getTeams();
    }

    @Test
    public void testSaveTeam() {
        Team expectedTeam = new Team(1L, "Team1");

        given(teamDao.saveTeam(expectedTeam)).willReturn(expectedTeam);

        assertThat(teamService.saveTeam(expectedTeam)).isEqualTo(expectedTeam);

        verify(teamDao, times(1)).saveTeam(expectedTeam);
    }

    @Test
    public void testDeleteTeam() {
        given(teamDao.deleteTeam(1L)).willReturn(true);

        assertThat(teamService.deleteTeam(1L)).isTrue();
    }

}
