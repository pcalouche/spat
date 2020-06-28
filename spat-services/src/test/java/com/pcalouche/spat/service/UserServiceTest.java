package com.pcalouche.spat.service;

import com.pcalouche.spat.AbstractServiceTest;
import com.pcalouche.spat.api.dto.RoleDto;
import com.pcalouche.spat.api.dto.UserDto;
import com.pcalouche.spat.api.dto.UserEditRequest;
import com.pcalouche.spat.entity.Role;
import com.pcalouche.spat.entity.User;
import com.pcalouche.spat.repository.RoleRepository;
import com.pcalouche.spat.repository.UserRepository;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
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

    @BeforeEach
    public void before() {
        userService = new UserServiceImpl(userRepository, roleRepository);

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
        assertThat(userService.findById(user1.getId()))
                .isEqualTo(
                        Optional.of(
                                UserDto.builder()
                                        .username(user1.getUsername())
                                        .build()
                        )
                );

        assertThat(userService.findById(user2.getId() + 42))
                .isEqualTo(Optional.empty());
    }

    @Test
    public void testFindByUsername() {
        assertThat(userService.findByUsername(user1.getUsername()))
                .isEqualTo(
                        Optional.of(
                                UserDto.builder()
                                        .username(user1.getUsername())
                                        .build()
                        )
                );

        assertThat(userService.findByUsername("bogus"))
                .isEqualTo(Optional.empty());
    }

    @Test
    public void testFindAll() {
        assertThat(userService.findAll()).containsOnly(
                UserDto.builder()
                        .username(user1.getUsername())
                        .build(),
                UserDto.builder()
                        .username(user2.getUsername())
                        .roleDtos(
                                Stream.of(
                                        RoleDto.map(adminRole)
                                ).collect(Collectors.toSet())
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

        Optional<User> userOptional = userRepository.findById(userDto.getId());
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
                .enabled(false)
                .accountNonLocked(false)
                .credentialsNonExpired(false)
                .accountNonExpired(false)
                .build();

        UserDto userDtoExpected = UserDto.builder()
                .username("newUsername")
                .enabled(false)
                .accountNonLocked(false)
                .credentialsNonExpired(false)
                .accountNonExpired(false)
                .build();

        Optional<UserDto> userDtoOptional = userService.update(user2.getId(), userEditRequest);
        assertThat(userDtoOptional).isPresent();
        assertThat(userDtoOptional.get()).isEqualTo(userDtoExpected);

        Optional<User> optionalUser = userRepository.findById(user2.getId());
        assertThat(optionalUser).isPresent();

        User user = optionalUser.get();
        assertThat(user.getUsername()).isEqualTo(userEditRequest.getUsername());
        assertThat(user.isAccountNonExpired()).isFalse();
        assertThat(user.isAccountNonLocked()).isFalse();
        assertThat(user.isCredentialsNonExpired()).isFalse();
        assertThat(user.isEnabled()).isFalse();
        assertThat(user.getRoles()).isEmpty();
    }

    @Test
    public void testDeleteById() {
        userService.delete(user1.getId());

        assertThat(userService.findAll()).hasSize(1);

        assertThat(userService.findAll()).containsOnly(
                UserDto.builder()
                        .username(user2.getUsername())
                        .roleDtos(
                                Stream.of(
                                        RoleDto.map(adminRole)
                                ).collect(Collectors.toSet())
                        )
                        .build()
        );
    }
}
