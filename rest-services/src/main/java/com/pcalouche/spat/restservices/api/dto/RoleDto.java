package com.pcalouche.spat.restservices.api.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RoleDto {
    @EqualsAndHashCode.Exclude
    private Long id;
    private String name;
}
