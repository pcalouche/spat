package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.EndpointMessages;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.service.UserService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.hamcrest.Matchers.is;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willAnswer;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = UserController.class)
public class UserControllerTest extends AbstractControllerTest {
    @MockBean
    protected UserService userService;
    private UserDto testUserDto1;
    private UserDto testUserDto2;

    @Before
    public void before() {
        Set<RoleDto> expectedRoleDtos = new HashSet<>();
        expectedRoleDtos.add(RoleDto.builder()
                .id(1L)
                .name("ROLE_USER")
                .build());
        testUserDto1 = UserDto.builder()
                .username("activeUser")
                .roles(expectedRoleDtos)
                .build();
        testUserDto2 = UserDto.builder()
                .username("jsmith")
                .roles(expectedRoleDtos)
                .build();
    }

    @Test
    public void testFindById() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(testUserDto1);

        mockMvc.perform(get(ApiEndpoints.USERS + "/" + testUserDto1.getUsername())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));

        verify(userService, Mockito.times(1)).findById(testUserDto1.getUsername());
    }

    @Test
    public void testFindByIdUserNotFound() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(null);

        mockMvc.perform(get(ApiEndpoints.USERS + "/" + testUserDto1.getUsername())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_USER_FOUND, testUserDto1.getUsername()))));

        verify(userService, Mockito.times(1)).findById(testUserDto1.getUsername());
    }

    @Test
    public void testFindAll() throws Exception {
        List<UserDto> testUserDtos = new ArrayList<>();
        testUserDtos.add(testUserDto1);
        testUserDtos.add(testUserDto2);
        given(userService.findAll()).willReturn(testUserDtos);

        mockMvc.perform(get(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDtos)));

        verify(userService, Mockito.times(1)).findAll();
    }


    @Test
    public void testCreate() throws Exception {
        given(userService.save(testUserDto1)).willReturn(testUserDto1);

        MockHttpServletRequestBuilder request = post(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testUserDto1));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));

        verify(userService, Mockito.times(1)).save(testUserDto1);
    }

    @Test
    public void testCreateRequiresAdminRole() throws Exception {
        given(userService.save(testUserDto1)).willReturn(testUserDto1);

        MockHttpServletRequestBuilder request = post(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testUserDto1));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());
    }

    @Test
    public void testUpdate() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(testUserDto1);
        given(userService.save(testUserDto1)).willReturn(testUserDto1);

        MockHttpServletRequestBuilder request = put(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testUserDto1));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));

        verify(userService, Mockito.times(1)).save(testUserDto1);
    }

    @Test
    public void testUpdateWhenUserNotFound() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(null);

        MockHttpServletRequestBuilder request = put(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testUserDto1));

        mockMvc.perform(request)
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_USER_FOUND, testUserDto1.getUsername()))));

        verify(userService, Mockito.times(0)).save(testUserDto1);
    }

    @Test
    public void testUpdateRequiresAdminRole() throws Exception {
        MockHttpServletRequestBuilder request = put(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testUserDto1));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());

        verify(userService, Mockito.times(0)).save(testUserDto1);
    }

    @Test
    public void testDelete() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(testUserDto1);
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userService).delete(testUserDto1.getUsername());

        mockMvc.perform(delete(String.format("%s/%s", ApiEndpoints.USERS, testUserDto1.getUsername()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(userService, Mockito.times(1)).delete(testUserDto1.getUsername());
    }

    @Test
    public void testDeleteWhenUserNotFound() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(null);
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userService).delete(testUserDto1.getUsername());

        mockMvc.perform(delete(String.format("%s/%s", ApiEndpoints.USERS, testUserDto1.getUsername()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_USER_FOUND, testUserDto1.getUsername()))));

        verify(userService, Mockito.times(0)).delete(testUserDto1.getUsername());
    }

    @Test
    public void testDeleteRequiresAdminRole() throws Exception {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userService).delete(testUserDto1.getUsername());

        mockMvc.perform(delete(String.format("%s/%s", ApiEndpoints.USERS, testUserDto1.getUsername()))
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isForbidden());
    }
}
