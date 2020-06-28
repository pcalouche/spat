package com.pcalouche.spat.api.dto;

import com.pcalouche.spat.entity.Role;
import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RoleDto {
    @EqualsAndHashCode.Exclude
    private Integer id;
    private String name;

    public static RoleDto map(Role role) {
        return RoleDto.builder()
                .id(role.getId())
                .name(role.getName())
                .build();
    }
}
