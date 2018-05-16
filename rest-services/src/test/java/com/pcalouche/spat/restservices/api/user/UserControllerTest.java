package com.pcalouche.spat.restservices.api.user;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;

@WebMvcTest(value = UserController.class)
public class UserControllerTest extends AbstractControllerTest {

    @Test
    public void testFindAll() throws Exception {
        //        List<User> expectedUsers = new ArrayList<>();
        //        expectedUsers.add(new User(1L, "pcalouche", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER"))));
        //        expectedUsers.add(new User(2L, "jsmith", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER"))));
        //
        //        List<UserDto> expectedUserDtos = new ArrayList<>();
        //        expectedUserDtos.add(new UserDto(1L, "pcalouche", new HashSet<>(Collections.singletonList("ROLE_USER"))));
        //        expectedUserDtos.add(new UserDto(2L, "jsmith", new HashSet<>(Collections.singletonList("ROLE_USER"))));
        //
        //        given(userService.getUsers()).willReturn(expectedUsers);
        //
        //        mockMvc.perform(get(UserEndpoints.ROOT)
        //                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
        //                .andExpect(status().isOk())
        //                .andExpect(content().json(objectMapper.writeValueAsString(expectedUserDtos)));
        //
        //        verify(userService, Mockito.times(1)).getUsers();
    }

    @Test
    public void testFindByUserName() throws Exception {
        //        User expectedUser = new User(1L, "activeUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        //
        //        UserDto expectedUserDto = new UserDto(1L, "activeUser", new HashSet<>(Collections.singletonList("ROLE_USER")));
        //
        //        given(userService.findByUsername(expectedUser.getUsername())).willReturn(expectedUser);
        //
        //        mockMvc.perform(get(UserEndpoints.ROOT + "/" + expectedUser.getUsername())
        //                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
        //                .andExpect(status().isOk())
        //                .andExpect(content().json(objectMapper.writeValueAsString(expectedUserDto)));
        //
        //        verify(userService, Mockito.times(1)).findByUsername(expectedUser.getUsername());
    }

    @Test
    public void testFindByUserNameThrowsResourceNotFoundException() throws Exception {
        //        User expectedUser = new User(1L, "activeUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        //
        //        given(userService.findByUsername(expectedUser.getUsername())).willReturn(null);
        //
        //        mockMvc.perform(get(UserEndpoints.ROOT + "/" + expectedUser.getUsername())
        //                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
        //                .andExpect(status().isNotFound())
        //                .andExpect(jsonPath("$.message", is(String.format("User with %s not found", expectedUser.getUsername()))));
        //
        //        verify(userService, Mockito.times(1)).findByUsername(expectedUser.getUsername());
    }

    @Test
    public void testSave() throws Exception {
        //        User expectedUser = new User(1L, "pcalouche", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        //
        //        UserDto expectedUserDto = new UserDto(1L, "pcalouche", new HashSet<>(Collections.singletonList("ROLE_USER")));
        //
        //        given(userService.saveUser(expectedUser)).willReturn(expectedUser);
        //
        //        MockHttpServletRequestBuilder request = post(UserEndpoints.ROOT)
        //                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
        //                .contentType(MediaType.APPLICATION_JSON)
        //                .content(objectMapper.writeValueAsString(expectedUserDto));
        //
        //        mockMvc.perform(request)
        //                .andExpect(status().isOk())
        //                .andExpect(content().json(objectMapper.writeValueAsString(expectedUserDto)))
        //                .andReturn();
        //
        //        verify(userService, Mockito.times(1)).saveUser(expectedUser);
    }

    @Test
    public void testSaveRequiresAdminRole() throws Exception {
        //        User expectedUser = new User(1L, "pcalouche", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        //
        //        UserDto expectedUserDto = new UserDto(1L, "pcalouche", new HashSet<>(Collections.singletonList("ROLE_USER")));
        //
        //        given(userService.saveUser(expectedUser)).willReturn(expectedUser);
        //
        //        MockHttpServletRequestBuilder request = post(UserEndpoints.ROOT)
        //                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
        //                .contentType(MediaType.APPLICATION_JSON)
        //                .content(objectMapper.writeValueAsString(expectedUserDto));
        //
        //        mockMvc.perform(request)
        //                .andExpect(status().isForbidden())
        //                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON_UTF8))
        //                .andReturn();
    }

    @Test
    public void testDelete() throws Exception {
        //        given(userService.deleteUser(1L)).willReturn(true);
        //
        //        mockMvc.perform(delete(String.format("%s/%d", UserEndpoints.ROOT, 1L))
        //                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
        //                .andExpect(status().isOk())
        //                .andExpect(content().string(Boolean.TRUE.toString()));
        //
        //        verify(userService, Mockito.times(1)).deleteUser(1L);
    }

    @Test
    public void testDeleteOfNotFound() throws Exception {
        //        given(userService.deleteUser(1L)).willReturn(false);
        //
        //        mockMvc.perform(delete(String.format("%s/%d", UserEndpoints.ROOT, 1L))
        //                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
        //                .andExpect(status().isOk())
        //                .andExpect(content().string(Boolean.FALSE.toString()));
        //
        //        verify(userService, Mockito.times(1)).deleteUser(1L);
    }

    @Test
    public void testDeleteRequiresAdminRole() throws Exception {
        //        given(userService.deleteUser(1L)).willReturn(true);
        //
        //        mockMvc.perform(delete(String.format("%s/%d", UserEndpoints.ROOT, 1L))
        //                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
        //                .andExpect(status().isForbidden())
        //                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON_UTF8));
    }
}
