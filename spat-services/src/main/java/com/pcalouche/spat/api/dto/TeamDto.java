package com.pcalouche.spat.api.dto;

import com.pcalouche.spat.entity.Team;
import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TeamDto {
    @EqualsAndHashCode.Exclude
    private Integer id;
    private String name;

    public static TeamDto map(Team team) {
        return TeamDto.builder()
                .id(team.getId())
                .name(team.getName())
                .build();
    }
}
