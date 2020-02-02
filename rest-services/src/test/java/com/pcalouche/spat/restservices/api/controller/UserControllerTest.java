package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.EndpointMessages;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.dto.UserEditRequest;
import com.pcalouche.spat.restservices.service.UserService;
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
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
        testUserDto1 = UserDto.builder()
                .username("activeUser")
                .build();
        testUserDto2 = UserDto.builder()
                .username("jsmith")
                .build();
    }

    @Test
    public void testFindById() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(Optional.of(testUserDto1));

        mockMvc.perform(get(ApiEndpoints.USERS + "/" + testUserDto1.getUsername())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));

        verify(userService, Mockito.times(1)).findById(testUserDto1.getUsername());
    }

    @Test
    public void testFindByIdUserNotFound() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(Optional.empty());

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
        Set<RoleDto> roleDtos = Stream.of(
                RoleDto.builder().name("Admin").build()
        ).collect(Collectors.toSet());

        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("activeUser")
                .roleDtos(roleDtos)
                .build();

        given(userService.create(userEditRequest)).willReturn(testUserDto1);

        MockHttpServletRequestBuilder request = post(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));

        verify(userService, Mockito.times(1)).create(userEditRequest);
    }

    @Test
    public void testCreateRequiresAdminRole() throws Exception {
        Set<RoleDto> roleDtos = Stream.of(
                RoleDto.builder().name("Admin").build()
        ).collect(Collectors.toSet());

        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("activeUser")
                .roleDtos(roleDtos)
                .build();

        given(userService.create(userEditRequest)).willReturn(testUserDto1);

        MockHttpServletRequestBuilder request = post(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());

        verify(userService, Mockito.times(0)).create(userEditRequest);
    }

    @Test
    public void testUpdate() throws Exception {
        Set<RoleDto> roleDtos = Stream.of(
                RoleDto.builder().name("Admin").build()
        ).collect(Collectors.toSet());

        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("activeUser")
                .roleDtos(roleDtos)
                .build();

        given(userService.findById(userEditRequest.getUsername())).willReturn(Optional.of(testUserDto1));
        given(userService.update(userEditRequest.getUsername(), userEditRequest)).willReturn(Optional.of(testUserDto1));

        MockHttpServletRequestBuilder request = put(ApiEndpoints.USERS + "/" + userEditRequest.getUsername())
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));

        verify(userService, Mockito.times(1)).update(userEditRequest.getUsername(), userEditRequest);
    }

    @Test
    public void testUpdateWhenUserNotFound() throws Exception {
        Set<RoleDto> roleDtos = Stream.of(
                RoleDto.builder().name("Admin").build()
        ).collect(Collectors.toSet());

        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("activeUser")
                .roleDtos(roleDtos)
                .build();

        given(userService.findById(testUserDto1.getUsername())).willReturn(Optional.empty());

        MockHttpServletRequestBuilder request = put(ApiEndpoints.USERS + "/" + userEditRequest.getUsername())
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_USER_FOUND, testUserDto1.getUsername()))));
    }

    @Test
    public void testUpdateRequiresAdminRole() throws Exception {
        Set<RoleDto> roleDtos = Stream.of(
                RoleDto.builder().name("Admin").build()
        ).collect(Collectors.toSet());

        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("activeUser")
                .roleDtos(roleDtos)
                .build();

        MockHttpServletRequestBuilder request = put(ApiEndpoints.USERS + "/" + userEditRequest.getUsername())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());
    }

    @Test
    public void testDelete() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(Optional.of(testUserDto1));
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userService).delete(testUserDto1.getUsername());

        mockMvc.perform(delete(String.format("%s/%s", ApiEndpoints.USERS, testUserDto1.getUsername()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(userService, Mockito.times(1)).delete(testUserDto1.getUsername());
    }

    @Test
    public void testDeleteWhenUserNotFound() throws Exception {
        given(userService.findById(testUserDto1.getUsername())).willReturn(Optional.empty());
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userService).delete(testUserDto1.getUsername());

        mockMvc.perform(delete(String.format("%s/%s", ApiEndpoints.USERS, testUserDto1.getUsername()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_USER_FOUND, testUserDto1.getUsername()))));

        verify(userService, Mockito.times(0)).delete(testUserDto1.getUsername());
    }

    @Test
    public void testDeleteRequiresAdminRole() throws Exception {
        mockMvc.perform(delete(String.format("%s/%s", ApiEndpoints.USERS, testUserDto1.getUsername()))
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isForbidden());

        verify(userService, Mockito.times(0)).delete("activeUser");
    }
}
