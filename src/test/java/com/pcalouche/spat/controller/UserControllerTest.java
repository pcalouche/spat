package com.pcalouche.spat.controller;

import com.pcalouche.spat.controller.user.UserController;
import com.pcalouche.spat.controller.user.UserControllerUris;
import com.pcalouche.spat.model.User;
import com.pcalouche.spat.service.user.UserService;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class UserControllerTest {
    private final UserService userService = Mockito.mock(UserService.class);
    private final UserController userController = new UserController(userService);
    private final MockMvc mockMvc = MockMvcBuilders.standaloneSetup(userController).build();
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    public void getUsersTest() throws Exception {
        List<User> expectedUsers = new ArrayList<>();
        expectedUsers.add(new User(1L, "Philip", "Calouche"));
        expectedUsers.add(new User(2L, "Joe", "Smith"));

        Mockito.when(userService.getUsers()).thenReturn(expectedUsers);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(UserControllerUris.ROOT);
        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        List<User> actualUsers = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<List<User>>() {
        });
        Assert.assertEquals(actualUsers.size(), expectedUsers.size());
        for (int i = 0; i < actualUsers.size(); i++) {
            Assert.assertEquals(actualUsers.get(i), expectedUsers.get(i));
        }
    }

    @Test
    public void saveUserTest() throws Exception {
        User expectedUser = new User(1L, "Philip", "Calouche");

        Mockito.when(userService.saveUser(expectedUser)).thenReturn(expectedUser);

        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(UserControllerUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        User actualUser = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), User.class);
        Assert.assertEquals(actualUser, expectedUser);
    }

    @Test
    public void deleteUserTest() throws Exception {
        Mockito.when(userService.deleteUser(1L)).thenReturn(true);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.delete(String.format("%s/%d", UserControllerUris.ROOT, 1L));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        Assert.assertTrue(Boolean.valueOf(mvcResult.getResponse().getContentAsString()));
    }
}
