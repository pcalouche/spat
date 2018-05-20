package com.pcalouche.spat.restservices.api.entity;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class TeamTest extends AbstractModelMapperTest {

    @Test
    public void testTeamModelMapper() {
        TeamDto teamDto = new TeamDto(1L, "team1");
        Team team = modelMapper.map(teamDto, Team.class);

        assertThat(team.getId()).isEqualTo(1L);
        assertThat(team.getName()).isEqualTo("team1");
    }
}
