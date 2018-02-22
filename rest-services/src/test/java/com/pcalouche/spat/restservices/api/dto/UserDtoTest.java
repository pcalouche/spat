package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.entity.User;
import org.junit.Test;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

public class UserDtoTest extends AbstractModelMapperTest {

    @Test
    public void testUserModelMapper() {
        List<SimpleGrantedAuthority> simpleGrantedAuthorities = new ArrayList<>();
        simpleGrantedAuthorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
        simpleGrantedAuthorities.add(new SimpleGrantedAuthority("ROLE_USER"));

        User user = new User(1L, "username", simpleGrantedAuthorities);
        UserDto userDto = modelMapper.map(user, UserDto.class);

        assertThat(userDto.getId()).isEqualTo(1L);
        assertThat(userDto.getUsername()).isEqualTo("username");
        assertThat(userDto.getAuthorities()).containsExactly("ROLE_ADMIN", "ROLE_USER");

        user = new User(1L, "username", null);
        userDto = modelMapper.map(user, UserDto.class);
        assertThat(userDto.getId()).isEqualTo(1L);
        assertThat(userDto.getUsername()).isEqualTo("username");
        assertThat(userDto.getAuthorities()).isNull();
    }
}
