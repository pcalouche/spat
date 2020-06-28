package com.pcalouche.spat.api.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.pcalouche.spat.entity.User;
import lombok.*;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserDto {
    @EqualsAndHashCode.Exclude
    private Integer id;
    private String username;
    @Builder.Default
    private boolean accountNonExpired = true;
    @Builder.Default
    private boolean accountNonLocked = true;
    @Builder.Default
    private boolean credentialsNonExpired = true;
    @Builder.Default
    private boolean enabled = true;
    @JsonProperty("roles")
    @Builder.Default
    private Set<RoleDto> roleDtos = new HashSet<>();

    public static UserDto map(User user) {
        return UserDto.builder()
                .id(user.getId())
                .username(user.getUsername())
                .accountNonExpired(user.isAccountNonExpired())
                .accountNonLocked(user.isAccountNonLocked())
                .credentialsNonExpired(user.isCredentialsNonExpired())
                .enabled(user.isEnabled())
                .roleDtos(
                        user.getRoles().stream()
                                .map(RoleDto::map)
                                .collect(Collectors.toSet())
                )
                .build();
    }
}