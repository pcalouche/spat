package com.pcalouche.spat.restservices.api.entity;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import org.junit.Test;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.Arrays;

import static org.assertj.core.api.Assertions.assertThat;

public class UserTest extends AbstractModelMapperTest {
    @Test
    public void testUserDtoModelMapper() {
        UserDto userDto = new UserDto(1L, "username", Arrays.asList("ROLE_USER", "ROLE_ADMIN"));
        User user = modelMapper.map(userDto, User.class);

        assertThat(user.getId()).isEqualTo(1L);
        assertThat(user.getUsername()).isEqualTo("username");
        assertThat(user.getAuthorities()).containsExactly(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));

        userDto = new UserDto(1L, "username", null);
        user = modelMapper.map(userDto, User.class);
        assertThat(user.getId()).isEqualTo(1L);
        assertThat(user.getUsername()).isEqualTo("username");
        assertThat(user.getAuthorities()).isNull();
    }
}
