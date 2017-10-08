package com.pcalouche.spat.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.controller.user.UserController;
import com.pcalouche.spat.controller.user.UserUris;
import com.pcalouche.spat.model.User;
import com.pcalouche.spat.service.user.UserService;
import org.assertj.core.api.Assertions;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.Matchers;
import org.mockito.Mockito;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@WebMvcTest(value = UserController.class)
public class ControllerExceptionAdviceTest extends AbstractControllerTest {
    @MockBean
    private UserService userService;

    @Test
    public void testSecurityExceptionAdvice() throws Exception {
        SecurityException exception = new SecurityException("AUTH_TOKEN is invalid");
        JsonNode jsonNode = ControllerExceptionAdvice.buildErrorObject(exception);
        BDDMockito.given(authorizationInterceptor.preHandle(Matchers.any(), Matchers.any(), Matchers.any())).willThrow(exception);

        User expectedUser = new User(1L, "Test", "User");
        BDDMockito.given(userService.saveUser(expectedUser)).willReturn(expectedUser);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(UserUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(MockMvcResultMatchers.status().is5xxServerError())
                .andExpect(MockMvcResultMatchers.content().contentTypeCompatibleWith(MediaType.TEXT_PLAIN))
                .andExpect(MockMvcResultMatchers.content().json(jsonNode.toString()))
                .andReturn();

        Assertions.assertThat(mvcResult.getResolvedException()).isInstanceOf(SecurityException.class);

        Mockito.verify(authorizationInterceptor, Mockito.times(1)).preHandle(Matchers.any(), Matchers.any(), Matchers.any());
        Mockito.verify(userService, Mockito.times(0)).saveUser(expectedUser);
    }

    @Test
    public void testDataAccessExceptionAdvice() throws Exception {
        DataAccessException exception = new EmptyResultDataAccessException(1);
        JsonNode jsonNode = ControllerExceptionAdvice.buildErrorObject(exception);
        User expectedUser = new User(1L, "Test", "User");
        BDDMockito.given(userService.saveUser(expectedUser)).willThrow(exception);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(UserUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(MockMvcResultMatchers.status().is5xxServerError())
                .andExpect(MockMvcResultMatchers.content().contentTypeCompatibleWith(MediaType.TEXT_PLAIN))
                .andExpect(MockMvcResultMatchers.content().json(jsonNode.toString()))
                .andReturn();

        Assertions.assertThat(mvcResult.getResolvedException()).isInstanceOf(DataAccessException.class);

        Mockito.verify(userService, Mockito.times(1)).saveUser(expectedUser);
    }

    @Test
    public void testExceptionAdvice() throws Exception {
        Exception exception = new RuntimeException("Inducted Runtime Exception");
        JsonNode jsonNode = ControllerExceptionAdvice.buildErrorObject(exception);
        User expectedUser = new User(1L, "Test", "User");
        BDDMockito.given(userService.saveUser(expectedUser)).willThrow(exception);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(UserUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(MockMvcResultMatchers.status().is5xxServerError())
                .andExpect(MockMvcResultMatchers.content().contentTypeCompatibleWith(MediaType.TEXT_PLAIN))
                .andExpect(MockMvcResultMatchers.content().json(jsonNode.toString()))
                .andReturn();

        Assertions.assertThat(mvcResult.getResolvedException()).isInstanceOf(RuntimeException.class);

        Mockito.verify(userService, Mockito.times(1)).saveUser(expectedUser);
    }
}
