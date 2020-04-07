package com.pcalouche.spat.api.controller;

import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.api.EndpointMessages;
import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.api.dto.RoleDto;
import com.pcalouche.spat.api.dto.UserDto;
import com.pcalouche.spat.api.dto.UserEditRequest;
import com.pcalouche.spat.service.UserService;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.Matchers.is;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = UserController.class)
public class UserControllerTest extends AbstractControllerTest {
    @MockBean
    protected UserService userService;
    private UserDto testUserDto1;
    private UserDto testUserDto2;

    @BeforeEach
    public void before() {
        testUserDto1 = UserDto.builder()
                .id(1)
                .username("activeUser")
                .build();
        testUserDto2 = UserDto.builder()
                .id(2)
                .username("jsmith")
                .build();
    }

    @Test
    public void testCurrentUser() throws Exception {
        given(userService.findByUsername(testUserDto1.getUsername())).willReturn(Optional.of(testUserDto1));

        mockMvc.perform(get(Endpoints.USERS + "/current-user")
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));
    }

    @Test
    public void testFindById() throws Exception {
        given(userService.findById(testUserDto1.getId())).willReturn(Optional.of(testUserDto1));

        mockMvc.perform(get(Endpoints.USERS + "/" + testUserDto1.getId())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));
    }

    @Test
    public void testFindByIdUserNotFound() throws Exception {
        given(userService.findById(testUserDto1.getId())).willReturn(Optional.empty());

        mockMvc.perform(get(Endpoints.USERS + "/" + testUserDto1.getId())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isNotFound())
                .andExpect(MockMvcResultMatchers.jsonPath("$.message", Matchers.is(String.format(EndpointMessages.NO_USER_FOUND, testUserDto1.getId()))));
    }

    @Test
    public void testFindAll() throws Exception {
        List<UserDto> testUserDtos = Stream.of(
                testUserDto1,
                testUserDto2
        ).collect(Collectors.toList());

        given(userService.findAll()).willReturn(testUserDtos);

        mockMvc.perform(get(Endpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDtos)));
    }

    @Test
    public void testCreate() throws Exception {
        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("activeUser")
                .roleDtos(
                        Stream.of(
                                RoleDto.builder().name("Admin").build()
                        ).collect(Collectors.toSet())
                )
                .build();

        given(userService.create(userEditRequest)).willReturn(testUserDto1);

        MockHttpServletRequestBuilder request = post(Endpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));
    }

    @Test
    public void testCreateRequiresAdminRole() throws Exception {
        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("activeUser")
                .roleDtos(
                        Stream.of(
                                RoleDto.builder().name("Admin").build()
                        ).collect(Collectors.toSet())
                )
                .build();

        given(userService.create(userEditRequest)).willReturn(testUserDto1);

        MockHttpServletRequestBuilder request = post(Endpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());
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

        given(userService.findById(1)).willReturn(Optional.of(testUserDto1));
        given(userService.update(1, userEditRequest)).willReturn(Optional.of(testUserDto1));

        MockHttpServletRequestBuilder request = put(Endpoints.USERS + "/1")
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testUserDto1)));

        verify(userService, Mockito.times(1)).update(1, userEditRequest);
    }

    @Test
    public void testUpdateHandlesUserNotFound() throws Exception {
        Set<RoleDto> roleDtos = Stream.of(
                RoleDto.builder().name("Admin").build()
        ).collect(Collectors.toSet());

        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("activeUser")
                .roleDtos(roleDtos)
                .build();

        given(userService.findById(1)).willReturn(Optional.empty());

        MockHttpServletRequestBuilder request = put(Endpoints.USERS + "/1")
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_USER_FOUND, 1))));
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

        MockHttpServletRequestBuilder request = put(Endpoints.USERS + "/" + testUserDto1.getId())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());
    }

    @Test
    public void testDelete() throws Exception {
        given(userService.findById(testUserDto1.getId())).willReturn(Optional.of(testUserDto1));

        mockMvc.perform(delete(String.format("%s/%s", Endpoints.USERS, testUserDto1.getId()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(userService, Mockito.times(1)).delete(testUserDto1.getId());
    }

    @Test
    public void testDeleteWhenUserNotFound() throws Exception {
        given(userService.findById(testUserDto1.getId())).willReturn(Optional.empty());

        mockMvc.perform(delete(String.format("%s/%s", Endpoints.USERS, testUserDto1.getId()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_USER_FOUND, testUserDto1.getId()))));

        verify(userService, Mockito.times(0)).delete(testUserDto1.getId());
    }

    @Test
    public void testDeleteRequiresAdminRole() throws Exception {
        mockMvc.perform(delete(String.format("%s/%d", Endpoints.USERS, testUserDto1.getId()))
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isForbidden());
    }
}
