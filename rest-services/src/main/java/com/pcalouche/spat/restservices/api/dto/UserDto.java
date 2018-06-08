package com.pcalouche.spat.restservices.api.dto;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Set;

@Data
@NoArgsConstructor
public class UserDto {
    @EqualsAndHashCode.Exclude
    private Long id;
    private String username;
    private boolean accountNonExpired = true;
    private boolean accountNonLocked = true;
    private boolean credentialsNonExpired = true;
    private boolean enabled = true;
    private Set<RoleDto> roles;

    public UserDto(Long id, String username, Set<RoleDto> roles) {
        this.id = id;
        this.username = username;
        this.roles = roles;
    }
}