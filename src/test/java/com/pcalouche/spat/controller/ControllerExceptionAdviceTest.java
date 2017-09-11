package com.pcalouche.spat.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.pcalouche.spat.ControllerTest;
import com.pcalouche.spat.controller.user.UserController;
import com.pcalouche.spat.controller.user.UserControllerUris;
import com.pcalouche.spat.model.User;
import com.pcalouche.spat.service.user.UserService;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = UserController.class)
public class ControllerExceptionAdviceTest extends ControllerTest {
    @MockBean
    private UserService userService;

    @Test
    public void testSecurityExceptionAdvice() throws Exception {
        SecurityException exception = new SecurityException("AUTH_TOKEN is invalid");
        JsonNode jsonNode = ControllerExceptionAdvice.buildErrorObject(exception);
        given(authorizationInterceptor.preHandle(any(), any(), any())).willThrow(exception);

        User expectedUser = new User(1L, "Test", "User");
        given(userService.saveUser(expectedUser)).willReturn(expectedUser);

        MockHttpServletRequestBuilder request = post(UserControllerUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().is5xxServerError())
                .andExpect(content().contentTypeCompatibleWith(MediaType.TEXT_PLAIN))
                .andExpect(content().json(jsonNode.toString()))
                .andReturn();

        assertThat(mvcResult.getResolvedException()).isInstanceOf(SecurityException.class);

        verify(authorizationInterceptor, times(1)).preHandle(any(), any(), any());
        verify(userService, times(0)).saveUser(expectedUser);
    }

    @Test
    public void testDataAccessExceptionAdvice() throws Exception {
        DataAccessException exception = new EmptyResultDataAccessException(1);
        JsonNode jsonNode = ControllerExceptionAdvice.buildErrorObject(exception);
        User expectedUser = new User(1L, "Test", "User");
        given(userService.saveUser(expectedUser)).willThrow(exception);

        MockHttpServletRequestBuilder request = post(UserControllerUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().is5xxServerError())
                .andExpect(content().contentTypeCompatibleWith(MediaType.TEXT_PLAIN))
                .andExpect(content().json(jsonNode.toString()))
                .andReturn();

        assertThat(mvcResult.getResolvedException()).isInstanceOf(DataAccessException.class);

        verify(userService, times(1)).saveUser(expectedUser);
    }

    @Test
    public void testExceptionAdvice() throws Exception {
        Exception exception = new RuntimeException("Inducted Runtime Exception");
        JsonNode jsonNode = ControllerExceptionAdvice.buildErrorObject(exception);
        User expectedUser = new User(1L, "Test", "User");
        given(userService.saveUser(expectedUser)).willThrow(exception);

        MockHttpServletRequestBuilder request = post(UserControllerUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUser));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().is5xxServerError())
                .andExpect(content().contentTypeCompatibleWith(MediaType.TEXT_PLAIN))
                .andExpect(content().json(jsonNode.toString()))
                .andReturn();

        assertThat(mvcResult.getResolvedException()).isInstanceOf(RuntimeException.class);

        verify(userService, times(1)).saveUser(expectedUser);
    }
}
