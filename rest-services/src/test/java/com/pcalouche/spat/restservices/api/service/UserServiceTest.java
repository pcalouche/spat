package com.pcalouche.spat.restservices.api.service;

import com.pcalouche.spat.restservices.AbstractServiceTest;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.repository.RoleRepository;
import com.pcalouche.spat.restservices.api.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

public class UserServiceTest extends AbstractServiceTest {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoleRepository roleRepository;
    private UserService userService;
    private Role userRole;
    private Role adminRole;
    private User user1;
    private User user2;

    @Before
    public void before() {
        userService = new UserServiceImpl(modelMapper, userRepository, roleRepository);

        userRole = roleRepository.save(Role.builder()
                .name("ROLE_USER")
                .build());

        adminRole = roleRepository.save(Role.builder()
                .name("ROLE_ADMIN")
                .build());

        user1 = userRepository.save(
                User.builder()
                        .username("pcalouche")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(userRole).collect(Collectors.toSet()))
                        .build());
        user2 = userRepository.save(
                User.builder()
                        .username("jsmith")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(userRole).collect(Collectors.toSet()))
                        .build()
        );
    }

    @Test
    public void testFindAll() {
        List<UserDto> expectedUserDtos = new ArrayList<>();
        expectedUserDtos.add(UserDto.builder()
                .id(user1.getId())
                .username("pcalouche")
                .roles(Stream.of(modelMapper.map(userRole, RoleDto.class))
                        .collect(Collectors.toSet()))
                .build());
        expectedUserDtos.add(UserDto.builder()
                .id(user2.getId())
                .username("jsmith")
                .roles(Stream.of(modelMapper.map(userRole, RoleDto.class))
                        .collect(Collectors.toSet()))
                .build());

        assertThat(userService.findAll()).isEqualTo(expectedUserDtos);
    }

    @Test
    public void testFindByUsername() {
        UserDto expectedUserDto = UserDto.builder()
                .id(user1.getId())
                .username("pcalouche")
                .roles(Stream.of(modelMapper.map(userRole, RoleDto.class))
                        .collect(Collectors.toSet()))
                .build();

        assertThat(userService.findByUsername("pcalouche")).isEqualTo(expectedUserDto);
    }

    @Test
    public void testSave() {
        UserDto expectedUserDto = UserDto.builder()
                .id(user1.getId())
                .username("pcalouche")
                .roles(Stream.of(
                        modelMapper.map(userRole, RoleDto.class),
                        modelMapper.map(adminRole, RoleDto.class))
                        .collect(Collectors.toSet()))
                .enabled(false)
                .build();

        assertThat(userService.save(expectedUserDto)).isEqualTo(expectedUserDto);
    }

    @Test
    public void testDeleteById() {
        userService.deleteById(user1.getId());

        assertThat(userService.findAll()).hasSize(1);

        assertThat(userService.findAll()).containsOnly(UserDto.builder()
                .id(user2.getId())
                .username("jsmith")
                .roles(Stream.of(modelMapper.map(userRole, RoleDto.class))
                        .collect(Collectors.toSet()))
                .build());
    }
}
