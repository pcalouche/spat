package com.pcalouche.spat.restservices.service;

import com.pcalouche.spat.restservices.AbstractServiceTest;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.dto.UserEditRequest;
import com.pcalouche.spat.restservices.entity.Role;
import com.pcalouche.spat.restservices.entity.User;
import com.pcalouche.spat.restservices.repository.RoleRepository;
import com.pcalouche.spat.restservices.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

public class UserServiceTest extends AbstractServiceTest {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoleRepository roleRepository;
    private UserService userService;
    private Role adminRole;
    private User user1;
    private User user2;

    @Before
    public void before() {
        userService = new UserServiceImpl(modelMapper, userRepository, roleRepository);

        adminRole = roleRepository.save(Role.builder()
                .name("Admin")
                .build());

        user1 = userRepository.save(
                User.builder()
                        .username("pcalouche")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .build());

        user2 = userRepository.save(
                User.builder()
                        .username("jsmith")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(adminRole).collect(Collectors.toSet()))
                        .build()
        );
    }

    @Test
    public void testFindById() {
        assertThat(userService.findById(user1.getUsername()))
                .isEqualTo(
                        Optional.of(
                                UserDto.builder()
                                        .username(user1.getUsername())
                                        .build()
                        )
                );
    }

    @Test
    public void testFindAll() {
        assertThat(userService.findAll()).containsOnly(
                UserDto.builder()
                        .username(user1.getUsername())
                        .build(),
                UserDto.builder()
                        .username(user2.getUsername())
                        .roleDtos(Stream.of(
                                modelMapper.map(adminRole, RoleDto.class))
                                .collect(Collectors.toSet())
                        )
                        .build()
        );
    }

    @Test
    public void testCreate() {
        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("newUser")
                .roleDtos(Stream.of(
                        RoleDto.builder().name("Admin").build()
                ).collect(Collectors.toSet()))
                .build();

        UserDto userDtoExpected = UserDto.builder()
                .username("newUser")
                .roleDtos(Stream.of(
                        RoleDto.builder().name("Admin").build()
                ).collect(Collectors.toSet()))
                .build();

        UserDto userDto = userService.create(userEditRequest);

        assertThat(userDto).isEqualTo(userDtoExpected);

        Optional<User> userOptional = userRepository.findById(userDto.getUsername());
        assertThat(userOptional).isPresent();

        User user = userOptional.get();
        assertThat(user.getUsername()).isEqualTo(userEditRequest.getUsername());
        assertThat(user.isAccountNonExpired()).isEqualTo(userEditRequest.isAccountNonExpired());
        assertThat(user.isAccountNonLocked()).isEqualTo(userEditRequest.isAccountNonLocked());
        assertThat(user.isCredentialsNonExpired()).isEqualTo(userEditRequest.isCredentialsNonExpired());
        assertThat(user.isEnabled()).isEqualTo(userEditRequest.isEnabled());
        assertThat(user.getRoles()).hasSize(1);
        assertThat(user.getRoles()).containsExactly(
                Role.builder().name("Admin").build()
        );
    }

    @Test
    public void testUpdate() {
        UserEditRequest userEditRequest = UserEditRequest.builder()
                .username("newUsername")
                .accountNonExpired(false)
                .build();

        UserDto userDtoExpected = UserDto.builder()
                .username("jsmith") // TODO be able to update username and make it an email
                .accountNonExpired(false)
                .build();

        Optional<UserDto> userDtoOptional = userService.update(user2.getUsername(), userEditRequest);
        assertThat(userDtoOptional).isPresent();
        assertThat(userDtoOptional.get()).isEqualTo(userDtoExpected);

        Optional<User> optionalUser = userRepository.findById(user2.getUsername());
        assertThat(optionalUser).isPresent();

        User user = optionalUser.get();
        assertThat(user.isAccountNonExpired()).isFalse();
        assertThat(user.isAccountNonLocked()).isTrue();
        assertThat(user.isCredentialsNonExpired()).isTrue();
        assertThat(user.isEnabled()).isTrue();
        assertThat(user.getRoles()).isEmpty();
    }

    @Test
    public void testDeleteById() {
        userService.delete(user1.getUsername());

        assertThat(userService.findAll()).hasSize(1);

        assertThat(userService.findAll()).containsOnly(
                UserDto.builder()
                        .username(user2.getUsername())
                        .roleDtos(Stream.of(
                                modelMapper.map(adminRole, RoleDto.class))
                                .collect(Collectors.toSet())
                        )
                        .build()
        );
    }
}
