package com.pcalouche.spat.restservices.api.team;

import com.pcalouche.spat.restservices.AbstractServiceTest;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.team.repository.TeamRepository;
import com.pcalouche.spat.restservices.api.team.service.TeamService;
import com.pcalouche.spat.restservices.api.team.service.TeamServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willAnswer;
import static org.mockito.Mockito.verify;

public class TeamServiceTest extends AbstractServiceTest {
    @MockBean
    private TeamRepository teamRepository;
    private TeamService teamService;

    @Before
    public void before() {
        teamService = new TeamServiceImpl(teamRepository, modelMapper);
    }

    @Test
    public void testFindAll() {
        List<Team> mockTeams = new ArrayList<>();
        mockTeams.add(new Team(1L, "Team1"));
        mockTeams.add(new Team(2L, "Team2"));

        List<TeamDto> expectedTeamDtos = new ArrayList<>();
        expectedTeamDtos.add(new TeamDto(1L, "Team1"));
        expectedTeamDtos.add(new TeamDto(2L, "Team2"));

        given(teamRepository.findAll()).willReturn(mockTeams);

        assertThat(teamService.findAll()).isEqualTo(expectedTeamDtos);

        verify(teamRepository, Mockito.times(1)).findAll();
    }


    @Test
    public void testSave() {
        Team mockTeam = new Team(1L, "Team1");
        TeamDto expectedTeamDto = new TeamDto(1L, "Team1");

        given(teamRepository.save(mockTeam)).willReturn(mockTeam);

        assertThat(teamService.save(expectedTeamDto)).isEqualTo(expectedTeamDto);

        verify(teamRepository, Mockito.times(1)).save(mockTeam);
    }

    @Test
    public void testDelete() {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(teamRepository).deleteById(1L);

        assertThat(teamService.delete(1L)).isTrue();
    }
}
