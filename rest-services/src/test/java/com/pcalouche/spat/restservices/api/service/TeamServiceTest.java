package com.pcalouche.spat.restservices.api.service;

import com.pcalouche.spat.restservices.AbstractServiceTest;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.repository.TeamRepository;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

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
    public void testFindById() {
        assertThat(teamService.findById(team1.getId()))
                .isEqualTo(TeamDto.builder()
                        .id(team1.getId())
                        .name("Team1")
                        .build());
    }

    @Test
    public void testFindAll() {
        assertThat(teamService.findAll()).containsOnly(
                TeamDto.builder()
                        .id(team1.getId())
                        .name("Team1")
                        .build(),
                TeamDto.builder()
                        .id(team2.getId())
                        .name("Team2")
                        .build()
        );
    }


    @Test
    public void testSave() {
        TeamDto teamDtoToSave = TeamDto.builder()
                .id(team1.getId())
                .name("Team1NewName")
                .build();

        assertThat(teamService.save(teamDtoToSave)).isEqualTo(
                TeamDto.builder()
                        .id(team1.getId())
                        .name("Team1NewName")
                        .build()
        );
    }

    @Test
    public void testDeleteById() {
        teamService.delete(team2.getId());

        assertThat(teamService.findAll()).hasSize(1);

        assertThat(teamService.findAll()).containsOnly(
                TeamDto.builder()
                        .id(team1.getId())
                        .name("Team1")
                        .build()
        );
    }
}
