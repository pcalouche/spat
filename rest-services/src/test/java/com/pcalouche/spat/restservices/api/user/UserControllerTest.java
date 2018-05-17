package com.pcalouche.spat.restservices.api.user;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.api.user.controller.UserEndpoints;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
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

    @Test
    public void testFindAll() throws Exception {
        Set<RoleDto> expectedRoleDtos = new HashSet<>();
        expectedRoleDtos.add(new RoleDto(1L, "ROLE_USER"));
        List<UserDto> expectedUserDtos = new ArrayList<>();
        expectedUserDtos.add(new UserDto(1L, "pcalouche", expectedRoleDtos));
        expectedUserDtos.add(new UserDto(2L, "jsmith", expectedRoleDtos));

        given(userService.findAll()).willReturn(expectedUserDtos);


        mockMvc.perform(get(UserEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(expectedUserDtos)));

        verify(userService, Mockito.times(1)).findAll();
    }

    @Test
    public void testFindByUserName() throws Exception {
        Set<RoleDto> expectedRoleDtos = new HashSet<>();
        expectedRoleDtos.add(new RoleDto(1L, "ROLE_USER"));
        UserDto expectedUserDto = new UserDto(1L, "activeUser", expectedRoleDtos);

        given(userService.findByUsername(expectedUserDto.getUsername())).willReturn(expectedUserDto);

        mockMvc.perform(get(UserEndpoints.ROOT + "/" + expectedUserDto.getUsername())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(expectedUserDto)));

        verify(userService, Mockito.times(1)).findByUsername(expectedUserDto.getUsername());
    }

    @Test
    public void testFindByUsernameThrowsResourceNotFoundException() throws Exception {
        Set<RoleDto> expectedRoleDtos = new HashSet<>();
        expectedRoleDtos.add(new RoleDto(1L, "ROLE_USER"));
        UserDto expectedUserDto = new UserDto(1L, "activeUser", expectedRoleDtos);

        given(userService.findByUsername(expectedUserDto.getUsername())).willReturn(null);

        mockMvc.perform(get(UserEndpoints.ROOT + "/" + expectedUserDto.getUsername())
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format("User with %s not found", expectedUserDto.getUsername()))));

        verify(userService, Mockito.times(1)).findByUsername(expectedUserDto.getUsername());
    }

    @Test
    public void testSave() throws Exception {
        Set<RoleDto> expectedRoleDtos = new HashSet<>();
        expectedRoleDtos.add(new RoleDto(1L, "ROLE_USER"));
        UserDto expectedUserDto = new UserDto(1L, "pcalouche", expectedRoleDtos);

        given(userService.save(expectedUserDto)).willReturn(expectedUserDto);

        MockHttpServletRequestBuilder request = post(UserEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUserDto));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(expectedUserDto)))
                .andReturn();

        verify(userService, Mockito.times(1)).save(expectedUserDto);
    }

    @Test
    public void testSaveRequiresAdminRole() throws Exception {
        Set<RoleDto> expectedRoleDtos = new HashSet<>();
        expectedRoleDtos.add(new RoleDto(1L, "ROLE_USER"));
        UserDto expectedUserDto = new UserDto(1L, "pcalouche", expectedRoleDtos);

        given(userService.save(expectedUserDto)).willReturn(expectedUserDto);

        MockHttpServletRequestBuilder request = post(UserEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedUserDto));

        mockMvc.perform(request)
                .andExpect(status().isForbidden())
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON_UTF8))
                .andReturn();
    }

    @Test
    public void testDelete() throws Exception {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userService).deleteById(1L);

        mockMvc.perform(delete(String.format("%s/%d", UserEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(userService, Mockito.times(1)).deleteById(1L);
    }

    @Test
    public void testDeleteByIdNotFound() throws Exception {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userService).deleteById(1L);

        mockMvc.perform(delete(String.format("%s/%d", UserEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(userService, Mockito.times(1)).deleteById(1L);
    }

    @Test
    public void testDeleteByIdRequiresAdminRole() throws Exception {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userService).deleteById(1L);

        mockMvc.perform(delete(String.format("%s/%d", UserEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isForbidden())
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON_UTF8));
    }
}
