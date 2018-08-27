package com.pcalouche.spat.restservices.api.entity;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import org.junit.Test;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.HashSet;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

public class UserTest extends AbstractModelMapperTest {

    @Test
    public void testUserDtoModelMapper() {
        Set<RoleDto> roleDtos = new HashSet<>();
        roleDtos.add(RoleDto.builder()
                .id(1L)
                .name("ROLE_USER")
                .build());
        roleDtos.add(RoleDto.builder()
                .id(2L)
                .name("ROLE_ADMIN")
                .build());
        UserDto userDto = UserDto.builder()
                .id(1L)
                .username("username")
                .roles(roleDtos)
                .accountNonExpired(false)
                .enabled(false)
                .build();

        User user = modelMapper.map(userDto, User.class);

        assertThat(user.getId()).isEqualTo(1L);
        assertThat(user.getUsername()).isEqualTo("username");
        Set<Role> roles = new HashSet<>();
        roles.add(Role.builder()
                .id(1L)
                .name("ROLE_USER")
                .build());
        roles.add(Role.builder()
                .id(2L)
                .name("ROLE_ADMIN")
                .build());
        assertThat(user.getRoles()).isEqualTo(roles);
        assertThat(user.getAuthorities()).contains(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
        assertThat(user.isAccountNonExpired()).isFalse();
        assertThat(user.isAccountNonLocked()).isTrue();
        assertThat(user.isCredentialsNonExpired()).isTrue();
        assertThat(user.isEnabled()).isFalse();

        userDto = UserDto.builder()
                .id(1L)
                .username("username")
                .build();

        user = modelMapper.map(userDto, User.class);

        assertThat(user.getId()).isEqualTo(1L);
        assertThat(user.getUsername()).isEqualTo("username");
        assertThat(user.getAuthorities()).isEmpty();
        assertThat(user.isAccountNonExpired()).isTrue();
        assertThat(user.isAccountNonLocked()).isTrue();
        assertThat(user.isCredentialsNonExpired()).isTrue();
        assertThat(user.isEnabled()).isTrue();
    }
}
