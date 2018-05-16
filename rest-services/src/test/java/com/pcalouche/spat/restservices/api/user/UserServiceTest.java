package com.pcalouche.spat.restservices.api.user;

import com.pcalouche.spat.restservices.AbstractServiceTest;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import com.pcalouche.spat.restservices.api.user.service.UserServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willAnswer;
import static org.mockito.Mockito.verify;

public class UserServiceTest extends AbstractServiceTest {
    @MockBean
    private UserRepository userRepository;
    private UserService userService;

    @Before
    public void before() {
        userService = new UserServiceImpl(userRepository, modelMapper);
    }

    @Test
    public void testFindAll() {
        Set<Role> mockRoles = new HashSet<>();
        mockRoles.add(new Role("ROLE_USER"));

        List<User> mockUsers = new ArrayList<>();
        mockUsers.add(new User(1L, "pcalouche", mockRoles));
        mockUsers.add(new User(2L, "jsmith", mockRoles));

        Set<RoleDto> expectedRoles = new HashSet<>();
        expectedRoles.add(new RoleDto(1L, "ROLE_USER"));
        List<UserDto> expectedUserDtos = new ArrayList<>();
        expectedUserDtos.add(new UserDto(1L, "pcalouche", expectedRoles));
        expectedUserDtos.add(new UserDto(2L, "jsmith", expectedRoles));

        given(userRepository.findAll()).willReturn(mockUsers);

        assertThat(userService.findAll()).isEqualTo(expectedUserDtos);

        verify(userRepository, Mockito.times(1)).findAll();
    }

    @Test
    public void testFindByUsername() {
        Set<Role> mockRoles = new HashSet<>();
        mockRoles.add(new Role("ROLE_USER"));
        User mockUser = new User(1L, "pcalouche", mockRoles);

        Set<RoleDto> expectedRoles = new HashSet<>();
        expectedRoles.add(new RoleDto(1L, "ROLE_USER"));
        UserDto expectedUserDto = new UserDto(1L, "pcalouche", expectedRoles);

        given(userRepository.findByUsername(mockUser.getUsername())).willReturn(mockUser);

        assertThat(userService.findByUsername(expectedUserDto.getUsername())).isEqualTo(expectedUserDto);

        verify(userRepository, Mockito.times(1)).findByUsername(mockUser.getUsername());
    }

    @Test
    public void testSave() {
        Set<Role> mockRoles = new HashSet<>();
        mockRoles.add(new Role("ROLE_USER"));
        User mockUser = new User(1L, "pcalouche", mockRoles);

        Set<RoleDto> expectedRoles = new HashSet<>();
        expectedRoles.add(new RoleDto(1L, "ROLE_USER"));
        UserDto expectedUserDto = new UserDto(1L, "pcalouche", expectedRoles);

        given(userRepository.save(mockUser)).willReturn(mockUser);

        assertThat(userService.save(expectedUserDto)).isEqualTo(expectedUserDto);

        verify(userRepository, Mockito.times(1)).save(mockUser);
    }

    @Test
    public void testDelete() {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(userRepository).deleteById(1L);

        assertThat(userService.delete(1L)).isTrue();
    }
}
