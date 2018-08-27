package com.pcalouche.spat.restservices.api.service;

import com.pcalouche.spat.restservices.AbstractServiceTest;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.repository.TeamRepository;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

public class TeamServiceTest extends AbstractServiceTest {
    @Autowired
    private TeamRepository teamRepository;
    private TeamService teamService;
    private Team team1;
    private Team team2;

    @Before
    public void before() {
        teamService = new TeamServiceImpl(modelMapper, teamRepository);

        team1 = teamRepository.save(Team.builder()
                .name("Team1")
                .build());

        team2 = teamRepository.save(Team.builder()
                .name("Team2")
                .build());
    }

    @Test
    public void testFindAll() {
        List<TeamDto> expectedTeamDtos = new ArrayList<>();
        expectedTeamDtos.add(TeamDto.builder()
                .id(team1.getId())
                .name("Team1")
                .build());
        expectedTeamDtos.add(TeamDto.builder()
                .id(team2.getId())
                .name("Team2")
                .build());

        assertThat(teamService.findAll()).isEqualTo(expectedTeamDtos);
    }


    @Test
    public void testSave() {
        TeamDto expectedTeamDto = TeamDto.builder()
                .id(team1.getId())
                .name("Team1NewName")
                .build();

        assertThat(teamService.save(expectedTeamDto)).isEqualTo(expectedTeamDto);
    }

    @Test
    public void testDeleteById() {
        teamService.deleteById(team2.getId());

        assertThat(teamService.findAll()).hasSize(1);

        assertThat(teamService.findAll()).containsOnly(TeamDto.builder()
                .id(team1.getId())
                .name("Team1")
                .build());
    }
}
