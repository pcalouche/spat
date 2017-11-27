package com.pcalouche.spat.api.team;

import com.pcalouche.spat.AbstractServiceTest;
import com.pcalouche.spat.api.model.Team;
import com.pcalouche.spat.api.team.dao.TeamDao;
import com.pcalouche.spat.api.team.service.TeamService;
import com.pcalouche.spat.api.team.service.TeamServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

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

        assertThat(teamService.getTeams()).isEqualTo(expectedTeams);

        Mockito.verify(teamDao, Mockito.times(1)).getTeams();
    }

    @Test
    public void testSaveTeam() {
        Team expectedTeam = new Team(1L, "Team1");

        BDDMockito.given(teamDao.saveTeam(expectedTeam)).willReturn(expectedTeam);

        assertThat(teamService.saveTeam(expectedTeam)).isEqualTo(expectedTeam);

        Mockito.verify(teamDao, Mockito.times(1)).saveTeam(expectedTeam);
    }

    @Test
    public void testDeleteTeam() {
        BDDMockito.given(teamDao.deleteTeam(1L)).willReturn(true);

        assertThat(teamService.deleteTeam(1L)).isTrue();
    }

}
