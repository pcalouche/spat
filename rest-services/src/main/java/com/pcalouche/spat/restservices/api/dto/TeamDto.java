package com.pcalouche.spat.restservices.api.dto;

import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TeamDto {
    @EqualsAndHashCode.Exclude
    private Integer id;
    private String name;
}
