package com.pcalouche.spat.restservices.api.user.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.exception.RestResourceNotFoundException;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@RestController
@RequestMapping(value = UserEndpoints.ROOT)
public class UserController extends AbstractSpatController {
    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    @GetMapping
    public List<UserDto> findAll() {
        return userService.findAll();
    }

    @GetMapping(value = "/{username}")
    public UserDto findByUsername(@PathVariable String username) throws RestResourceNotFoundException {
        UserDto userDto = userService.findByUsername(username);
        if (userDto == null) {
            throw new RestResourceNotFoundException(String.format("User with %s not found", username));
        }
        return userDto;
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PostMapping
    public UserDto save(@RequestBody UserDto userDto) {
        if (userDto.getRoles() == null || userDto.getRoles().isEmpty()) {
            Set<RoleDto> roles = new HashSet<>();
            roles.add(new RoleDto(1L, "ROLE_USER"));
        }
        return userService.save(userDto);
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @DeleteMapping(value = "/{id}")
    public boolean delete(@PathVariable Long id) {
        return userService.delete(id);
    }
}
