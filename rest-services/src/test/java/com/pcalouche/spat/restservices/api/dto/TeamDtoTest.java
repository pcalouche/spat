package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.entity.Team;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class TeamDtoTest extends AbstractModelMapperTest {

    @Test
    public void testTeamDtoModelMapper() {
        Team team = new Team(1L, "team1");
        TeamDto teamDto = modelMapper.map(team, TeamDto.class);

        assertThat(teamDto.getId()).isEqualTo(1L);
        assertThat(teamDto.getName()).isEqualTo("team1");
    }
}
