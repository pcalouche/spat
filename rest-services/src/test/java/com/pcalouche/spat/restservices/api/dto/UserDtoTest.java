package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import org.junit.Test;

public class UserDtoTest extends AbstractModelMapperTest {

    @Test
    public void testUserModelMapper() {
        //        List<SimpleGrantedAuthority> simpleGrantedAuthorities = new ArrayList<>();
        //        simpleGrantedAuthorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
        //        simpleGrantedAuthorities.add(new SimpleGrantedAuthority("ROLE_USER"));
        //
        //        User user = new User(1L, "username", simpleGrantedAuthorities);
        //        UserDto userDto = modelMapper.map(user, UserDto.class);
        //
        //        assertThat(userDto.getId()).isEqualTo(1L);
        //        assertThat(userDto.getUsername()).isEqualTo("username");
        //        assertThat(userDto.getRoles()).containsExactly("ROLE_ADMIN", "ROLE_USER");
        //
        //        user = new User(1L, "username", null);
        //        userDto = modelMapper.map(user, UserDto.class);
        //        assertThat(userDto.getId()).isEqualTo(1L);
        //        assertThat(userDto.getUsername()).isEqualTo("username");
        //        assertThat(userDto.getRoles()).isNull();
    }
}
