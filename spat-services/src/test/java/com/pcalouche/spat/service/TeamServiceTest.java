package com.pcalouche.spat.service;

import com.pcalouche.spat.AbstractServiceTest;
import com.pcalouche.spat.api.dto.TeamDto;
import com.pcalouche.spat.api.dto.TeamEditRequest;
import com.pcalouche.spat.entity.Team;
import com.pcalouche.spat.repository.TeamRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

public class TeamServiceTest extends AbstractServiceTest {
    @Autowired
    private TeamRepository teamRepository;
    private TeamService teamService;
    private Team team1;
    private Team team2;

    @BeforeEach
    public void before() {
        teamService = new TeamServiceImpl(teamRepository);

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
                .isEqualTo(
                        Optional.of(
                                TeamDto.builder()
                                        .id(team1.getId())
                                        .name("Team1")
                                        .build()
                        ));

        assertThat(teamService.findById(team2.getId() + 42))
                .isEqualTo(Optional.empty());
    }

    @Test
    public void testFindByName() {
        assertThat(teamService.findByName(team1.getName()))
                .isEqualTo(
                        Optional.of(
                                TeamDto.builder()
                                        .id(team1.getId())
                                        .name("Team1")
                                        .build()
                        ));

        assertThat(teamService.findByName("bogus"))
                .isEqualTo(Optional.empty());
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
    public void testCreate() {
        TeamEditRequest teamEditRequest = TeamEditRequest.builder()
                .name("Team1NewName")
                .build();

        TeamDto teamDtoExpected = TeamDto.builder()
                .name("Team1NewName")
                .build();

        TeamDto teamDto = teamService.create(teamEditRequest);

        assertThat(teamDto).isEqualTo(teamDtoExpected);

        Optional<Team> teamOptional = teamRepository.findById(teamDto.getId());
        assertThat(teamOptional).isPresent();

        Team team = teamOptional.get();
        assertThat(team.getName()).isEqualTo(teamEditRequest.getName());
    }

    @Test
    public void testUpdate() {
        TeamEditRequest teamEditRequest = TeamEditRequest.builder()
                .name("Team1NewName")
                .build();

        TeamDto teamDtoExpected = TeamDto.builder()
                .name("Team1NewName")
                .build();

        Optional<TeamDto> teamDtoOptional = teamService.update(team1.getId(), teamEditRequest);
        assertThat(teamDtoOptional).isPresent();
        assertThat(teamDtoOptional.get()).isEqualTo(teamDtoExpected);

        Optional<Team> teamOptional = teamRepository.findById(teamDtoOptional.get().getId());
        assertThat(teamOptional).isPresent();

        Team team = teamOptional.get();
        assertThat(team.getName())
                .isEqualTo(teamEditRequest.getName());
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
