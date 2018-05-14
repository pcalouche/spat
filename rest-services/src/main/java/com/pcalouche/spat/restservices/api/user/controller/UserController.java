package com.pcalouche.spat.restservices.api.user.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.exception.RestResourceNotFoundException;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import org.modelmapper.ModelMapper;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@RestController
@RequestMapping(value = UserEndpoints.ROOT)
public class UserController extends AbstractSpatController {
    private final UserService userService;
    private final ModelMapper modelMapper;

    public UserController(UserService userService,
                          ModelMapper modelMapper) {
        this.userService = userService;
        this.modelMapper = modelMapper;
    }

    @GetMapping
    public List<UserDto> getUsers() {
        return userService.getUsers()
                .stream()
                .map(user -> modelMapper.map(user, UserDto.class))
                .collect(Collectors.toList());
    }

    @GetMapping(value = "/{username}")
    public UserDto getByUsername(@PathVariable String username) throws RestResourceNotFoundException {
        User user = userService.getByUsername(username);
        if (user == null) {
            throw new RestResourceNotFoundException(String.format("User with %s not found", username));
        }
        return modelMapper.map(user, UserDto.class);
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PostMapping
    public UserDto saveUser(@RequestBody UserDto userDto) {
        if (userDto.getRoles() == null || userDto.getRoles().isEmpty()) {
            Set<RoleDto> roles = new HashSet<>();
            // TODO retrieve from database
            roles.add(new RoleDto(1L, "ROLE_USER"));
        }
        User user = modelMapper.map(userDto, User.class);
        return modelMapper.map(userService.saveUser(user), UserDto.class);
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @DeleteMapping(value = "/{id}")
    public boolean deleteUser(@PathVariable Long id) {
        return userService.deleteUser(id);
    }
}
