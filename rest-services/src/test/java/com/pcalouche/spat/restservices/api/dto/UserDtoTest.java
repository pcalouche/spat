package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import org.junit.Test;

import java.util.HashSet;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

public class UserDtoTest extends AbstractModelMapperTest {

    @Test
    public void testUserDtoModelMapper() {
        Set<Role> roles = new HashSet<>();
        Role role1 = Role.builder()
                .id(1L)
                .name("ROLE_ADMIN")
                .build();
        Role role2 = Role.builder()
                .id(2L)
                .name("ROLE_USER")
                .build();
        roles.add(role1);
        roles.add(role2);

        User user = User.builder()
                .username("user1")
                .password("password")
                .accountNonExpired(false)
                .enabled(false)
                .roles(roles)
                .build();

        UserDto userDto = modelMapper.map(user, UserDto.class);

        assertThat(userDto.getUsername()).isEqualTo("user1");
        assertThat(userDto.getRoles()).hasSize(2);
        assertThat(userDto.getRoles()).containsOnly(
                modelMapper.map(role1, RoleDto.class),
                modelMapper.map(role2, RoleDto.class)
        );
        assertThat(userDto.isAccountNonExpired()).isFalse();
        assertThat(userDto.isAccountNonLocked()).isTrue();
        assertThat(userDto.isCredentialsNonExpired()).isTrue();
        assertThat(userDto.isEnabled()).isFalse();

        user = User.builder()
                .username("user2")
                .build();

        userDto = modelMapper.map(user, UserDto.class);

        assertThat(userDto.getUsername()).isEqualTo("user2");
        assertThat(userDto.getRoles()).isEmpty();
        assertThat(userDto.isAccountNonExpired()).isTrue();
        assertThat(userDto.isAccountNonLocked()).isTrue();
        assertThat(userDto.isCredentialsNonExpired()).isTrue();
        assertThat(userDto.isEnabled()).isTrue();
    }
}
