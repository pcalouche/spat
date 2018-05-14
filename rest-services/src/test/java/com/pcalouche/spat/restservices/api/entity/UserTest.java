package com.pcalouche.spat.restservices.api.entity;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import org.junit.Test;

public class UserTest extends AbstractModelMapperTest {

    @Test
    public void testUserDtoModelMapper() {
        //        UserDto userDto = new UserDto(1L, "username", new HashSet<>(Arrays.asList("ROLE_USER", "ROLE_ADMIN")));
        //        User user = modelMapper.map(userDto, User.class);
        //
        //        assertThat(user.getId()).isEqualTo(1L);
        //        assertThat(user.getUsername()).isEqualTo("username");
        //        assertThat(user.getAuthorities()).containsExactly(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
        //
        //        userDto = new UserDto(1L, "username", null);
        //        user = modelMapper.map(userDto, User.class);
        //        assertThat(user.getId()).isEqualTo(1L);
        //        assertThat(user.getUsername()).isEqualTo("username");
        //        assertThat(user.getAuthorities()).isNull();
    }
}
