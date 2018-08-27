package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.exception.RestResourceNotFoundException;
import com.pcalouche.spat.restservices.api.service.UserService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Api(description = "User endpoints")
@RestController
@RequestMapping(value = ApiEndpoints.USER)
public class UserController extends AbstractSpatController {
    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    @ApiOperation(value = "Use to find all users")
    @GetMapping
    public List<UserDto> findAll() {
        return userService.findAll();
    }

    @ApiOperation(value = "Use to find a user by username")
    @GetMapping(value = "/{username}")
    public UserDto findByUsername(@PathVariable String username) throws RestResourceNotFoundException {
        UserDto userDto = userService.findByUsername(username);
        if (userDto == null) {
            throw new RestResourceNotFoundException(String.format("User with %s not found", username));
        }
        return userDto;
    }

    @ApiOperation(value = "Use to save a user")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PostMapping
    public UserDto save(@RequestBody UserDto userDto) {
        return userService.save(userDto);
    }

    @ApiOperation(value = "Use to delete a user")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @DeleteMapping(value = "/{id}")
    public void deleteById(@PathVariable Long id) {
        userService.deleteById(id);
    }
}
