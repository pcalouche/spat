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
        Role role1 = new Role(1L, "ROLE_ADMIN");
        Role role2 = new Role(1L, "ROLE_USER");
        roles.add(role1);
        roles.add(role2);

        User user = new User(1L, "username", roles);
        user.setPassword("password");
        user.setAccountNonExpired(false);
        user.setEnabled(false);
        UserDto userDto = modelMapper.map(user, UserDto.class);

        assertThat(userDto.getId()).isEqualTo(1L);
        assertThat(userDto.getUsername()).isEqualTo("username");
        assertThat(userDto.getRoles()).hasSize(2);
        assertThat(userDto.getRoles()).contains(new RoleDto(1L, "ROLE_ADMIN"));
        assertThat(userDto.getRoles()).contains(new RoleDto(1L, "ROLE_USER"));
        assertThat(userDto.isAccountNonExpired()).isFalse();
        assertThat(userDto.isAccountNonLocked()).isTrue();
        assertThat(userDto.isCredentialsNonExpired()).isTrue();
        assertThat(userDto.isEnabled()).isFalse();

        user = new User(1L, "username", new HashSet<>());
        userDto = modelMapper.map(user, UserDto.class);
        assertThat(userDto.getId()).isEqualTo(1L);
        assertThat(userDto.getUsername()).isEqualTo("username");
        assertThat(userDto.getRoles()).isEmpty();
        assertThat(userDto.isAccountNonExpired()).isTrue();
        assertThat(userDto.isAccountNonLocked()).isTrue();
        assertThat(userDto.isCredentialsNonExpired()).isTrue();
        assertThat(userDto.isEnabled()).isTrue();
    }
}
