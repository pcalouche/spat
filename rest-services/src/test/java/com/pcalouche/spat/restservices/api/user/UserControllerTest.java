package com.pcalouche.spat.restservices.api.user;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.api.user.controller.UserEndpoints;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

@WebMvcTest(value = UserController.class)
@WithMockUser
public class UserControllerTest extends AbstractControllerTest {
    @MockBean
    private UserService userService;

    @Test
    public void testGetUsers() throws Exception {
        List<User> expectedUsers = new ArrayList<>();
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
        expectedUsers.add(new User(1L, "pcalouche", authorities));
        expectedUsers.add(new User(2L, "jsmith", authorities));

        List<UserDto> expectedUserDtos = new ArrayList<>();
        expectedUserDtos.add(new UserDto(1L, "pcalouche", Collections.singletonList("ROLE_USER")));
        expectedUserDtos.add(new UserDto(2L, "jsmith", Collections.singletonList("ROLE_USER")));

        given(userService.getUsers()).willReturn(expectedUsers);

        mockMvc.perform(MockMvcRequestBuilders.get(UserEndpoints.ROOT))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(MockMvcResultMatchers.content().json(objectMapper.writeValueAsString(expectedUserDtos)));

        verify(userService, Mockito.times(1)).getUsers();
    }

    @Test
    public void testSaveUser() throws Exception {
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
        User expectedUser = new User(1L, "pcalouche", authorities);

        UserDto expectedUserDto = new UserDto(1L, "pcalouche", Collections.singletonList("ROLE_USER"));

        given(userService.saveUser(expectedUser)).willReturn(expectedUser);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(UserEndpoints.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUserDto));

        mockMvc.perform(request)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(MockMvcResultMatchers.content().json(objectMapper.writeValueAsString(expectedUserDto)))
                .andReturn();

        verify(userService, Mockito.times(1)).saveUser(expectedUser);
    }

    @Test
    public void testDeleteUser() throws Exception {
        given(userService.deleteUser(1L)).willReturn(true);

        mockMvc.perform(MockMvcRequestBuilders.delete(String.format("%s/%d", UserEndpoints.ROOT, 1L)))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().string(Boolean.TRUE.toString()));

        verify(userService, Mockito.times(1)).deleteUser(1L);
    }

    @Test
    public void testDeleteUserNotFound() throws Exception {
        given(userService.deleteUser(1L)).willReturn(false);

        mockMvc.perform(MockMvcRequestBuilders.delete(String.format("%s/%d", UserEndpoints.ROOT, 1L)))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().string(Boolean.FALSE.toString()));

        verify(userService, Mockito.times(1)).deleteUser(1L);
    }

}
