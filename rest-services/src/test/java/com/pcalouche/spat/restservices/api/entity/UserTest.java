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
        roleDtos.add(new RoleDto(1L, "ROLE_USER"));
        roleDtos.add(new RoleDto(1L, "ROLE_ADMIN"));
        UserDto userDto = new UserDto(1L, "username", roleDtos);
        userDto.setAccountNonExpired(false);
        userDto.setEnabled(false);
        User user = modelMapper.map(userDto, User.class);

        assertThat(user.getId()).isEqualTo(1L);
        assertThat(user.getUsername()).isEqualTo("username");
        Set<Role> roles = new HashSet<>();
        roles.add(new Role(1L, "ROLE_USER"));
        roles.add(new Role(2L, "ROLE_ADMIN"));
        assertThat(user.getRoles()).contains(new Role(1L, "ROLE_USER"), new Role(2L, "ROLE_ADMIN"));
        assertThat(user.getAuthorities()).contains(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
        assertThat(user.isAccountNonExpired()).isFalse();
        assertThat(user.isAccountNonLocked()).isTrue();
        assertThat(user.isCredentialsNonExpired()).isTrue();
        assertThat(user.isEnabled()).isFalse();

        userDto = new UserDto(1L, "username", new HashSet<>());
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
