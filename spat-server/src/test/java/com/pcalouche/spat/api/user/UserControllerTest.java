package com.pcalouche.spat.api.user;

import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.api.model.User;
import com.pcalouche.spat.api.user.controller.UserController;
import com.pcalouche.spat.api.user.controller.UserUris;
import com.pcalouche.spat.api.user.service.UserService;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.util.ArrayList;
import java.util.List;


@WebMvcTest(value = UserController.class)
public class UserControllerTest extends AbstractControllerTest {
    @MockBean
    private UserService userService;

    @Test
    public void testGetUsers() throws Exception {
        List<User> expectedUsers = new ArrayList<>();
        expectedUsers.add(new User(1L, "Philip", "Calouche"));
        expectedUsers.add(new User(2L, "Joe", "Smith"));

        BDDMockito.given(userService.getUsers()).willReturn(expectedUsers);

        mockMvc.perform(MockMvcRequestBuilders.get(UserUris.ROOT))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(MockMvcResultMatchers.content().json(objectMapper.writeValueAsString(expectedUsers)));

        Mockito.verify(userService, Mockito.times(1)).getUsers();
    }

    @Test
    public void testSaveUser() throws Exception {
        User expectedUser = new User(1L, "Philip", "Calouche");

        BDDMockito.given(userService.saveUser(expectedUser)).willReturn(expectedUser);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(UserUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        mockMvc.perform(request)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(MockMvcResultMatchers.content().json(objectMapper.writeValueAsString(expectedUser)));

        Mockito.verify(userService, Mockito.times(1)).saveUser(expectedUser);
    }

    @Test
    public void testDeleteUser() throws Exception {
        BDDMockito.given(userService.deleteUser(1L)).willReturn(true);

        mockMvc.perform(MockMvcRequestBuilders.delete(String.format("%s/%d", UserUris.ROOT, 1L)))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().string(Boolean.TRUE.toString()));

        Mockito.verify(userService, Mockito.times(1)).deleteUser(1L);
    }

    @Test
    public void testDeleteUserNotFound() throws Exception {
        BDDMockito.given(userService.deleteUser(1L)).willReturn(false);

        mockMvc.perform(MockMvcRequestBuilders.delete(String.format("%s/%d", UserUris.ROOT, 1L)))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().string(Boolean.FALSE.toString()));

        Mockito.verify(userService, Mockito.times(1)).deleteUser(1L);
    }

}
