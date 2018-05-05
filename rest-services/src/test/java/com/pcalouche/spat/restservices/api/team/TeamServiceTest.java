package com.pcalouche.spat.restservices.api.team;

import com.pcalouche.spat.restservices.AbstractServiceTest;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.team.dao.TeamDao;
import com.pcalouche.spat.restservices.api.team.repository.TeamRepository;
import com.pcalouche.spat.restservices.api.team.service.TeamService;
import com.pcalouche.spat.restservices.api.team.service.TeamServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

public class TeamServiceTest extends AbstractServiceTest {
    @MockBean
    private TeamDao teamDao;
    @MockBean
    private TeamRepository teamRepository;
    private TeamService teamService;

    @Before
    public void before() {
        teamService = new TeamServiceImpl(teamDao, teamRepository);
    }

    @Test
    public void testGetTeams() {
        List<Team> expectedTeams = new ArrayList<>();
        expectedTeams.add(new Team(1L, "Team1"));
        expectedTeams.add(new Team(2L, "Team2"));

        given(teamDao.getTeams()).willReturn(expectedTeams);

        assertThat(teamService.getTeams()).isEqualTo(expectedTeams);

        verify(teamDao, Mockito.times(1)).getTeams();
    }

    @Test
    public void testSaveTeam() {
        Team expectedTeam = new Team(1L, "Team1");

        given(teamDao.saveTeam(expectedTeam)).willReturn(expectedTeam);

        assertThat(teamService.saveTeam(expectedTeam)).isEqualTo(expectedTeam);

        verify(teamDao, Mockito.times(1)).saveTeam(expectedTeam);
    }

    @Test
    public void testDeleteTeam() {
        given(teamDao.deleteTeam(1L)).willReturn(true);

        assertThat(teamService.deleteTeam(1L)).isTrue();
    }

}
