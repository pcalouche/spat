package com.pcalouche.spat.api.dto;

import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RoleDto {
    @EqualsAndHashCode.Exclude
    private Integer id;
    private String name;
}
