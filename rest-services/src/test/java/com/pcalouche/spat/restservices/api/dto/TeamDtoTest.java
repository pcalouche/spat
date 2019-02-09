package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.entity.Team;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class TeamDtoTest extends AbstractModelMapperTest {

    @Test
    public void testTeamDtoModelMapper() {
        Team team = Team.builder()
                .id(1)
                .name("team1")
                .build();

        TeamDto teamDto = modelMapper.map(team, TeamDto.class);

        assertThat(teamDto.getId()).isEqualTo(1L);
        assertThat(teamDto.getName()).isEqualTo("team1");
    }
}
