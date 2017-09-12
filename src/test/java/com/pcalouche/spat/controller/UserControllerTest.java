package com.pcalouche.spat.controller;

import com.pcalouche.spat.ControllerTest;
import com.pcalouche.spat.controller.user.UserController;
import com.pcalouche.spat.controller.user.UserControllerUris;
import com.pcalouche.spat.model.User;
import com.pcalouche.spat.service.user.UserService;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@WebMvcTest(value = UserController.class)
public class UserControllerTest extends ControllerTest {
    @MockBean
    private UserService userService;

    @Test
    public void testGetUsers() throws Exception {
        List<User> expectedUsers = new ArrayList<>();
        expectedUsers.add(new User(1L, "Philip", "Calouche"));
        expectedUsers.add(new User(2L, "Joe", "Smith"));

        given(userService.getUsers()).willReturn(expectedUsers);

        mockMvc.perform(get(UserControllerUris.ROOT))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(content().json(objectMapper.writeValueAsString(expectedUsers)));

        verify(userService, times(1)).getUsers();
    }

    @Test
    public void testSaveUser() throws Exception {
        User expectedUser = new User(1L, "Philip", "Calouche");

        given(userService.saveUser(expectedUser)).willReturn(expectedUser);

        MockHttpServletRequestBuilder request = post(UserControllerUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(content().json(objectMapper.writeValueAsString(expectedUser)));

        verify(userService, times(1)).saveUser(expectedUser);
    }

    @Test
    public void testDeleteUser() throws Exception {
        given(userService.deleteUser(1L)).willReturn(true);

        mockMvc.perform(delete(String.format("%s/%d", UserControllerUris.ROOT, 1L)))
                .andExpect(status().isOk())
                .andExpect(content().string(Boolean.TRUE.toString()));

        verify(userService, times(1)).deleteUser(1L);
    }

    @Test
    public void testDeleteUserNotFound() throws Exception {
        given(userService.deleteUser(1L)).willReturn(false);

        mockMvc.perform(delete(String.format("%s/%d", UserControllerUris.ROOT, 1L)))
                .andExpect(status().isOk())
                .andExpect(content().string(Boolean.FALSE.toString()));

        verify(userService, times(1)).deleteUser(1L);
    }

}
