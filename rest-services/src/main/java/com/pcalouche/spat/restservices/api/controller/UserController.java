package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.EndpointMessages;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.exception.RestResourceForbiddenException;
import com.pcalouche.spat.restservices.api.exception.RestResourceNotFoundException;
import com.pcalouche.spat.restservices.api.service.UserService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Api(description = "User endpoints")
@RestController
@RequestMapping(value = ApiEndpoints.USERS)
public class UserController extends AbstractSpatController {
    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    @ApiOperation(value = "Find a user by username")
    @GetMapping(value = "/{username}")
    public UserDto findById(@PathVariable String username) throws RestResourceNotFoundException {
        UserDto userDto = userService.findById(username);
        if (userDto == null) {
            throw new RestResourceNotFoundException(String.format(EndpointMessages.NO_USER_FOUND, username));
        }
        return userDto;
    }

    @ApiOperation(value = "Find all users")
    @GetMapping
    public List<UserDto> findAll() {
        return userService.findAll();
    }

    @ApiOperation(value = "Create a new user")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PostMapping
    public UserDto create(@RequestBody UserDto userDto) {
        UserDto existingUserDto = userService.findById(userDto.getUsername());
        if (existingUserDto != null) {
            throw new RestResourceForbiddenException(String.format(EndpointMessages.USER_ALREADY_EXISTS, userDto.getUsername()));
        }
        return userService.save(userDto);
    }

    @ApiOperation(value = "Update an existing user")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PutMapping
    public UserDto update(@RequestBody UserDto userDto) {
        UserDto existingUserDto = userService.findById(userDto.getUsername());
        if (existingUserDto == null) {
            throw new RestResourceNotFoundException(String.format(EndpointMessages.NO_USER_FOUND, userDto.getUsername()));
        }
        return userService.save(userDto);
    }

    @ApiOperation(value = "Delete an existing user")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @DeleteMapping(value = "/{username}")
    public void delete(@PathVariable String username) {
        if (userService.findById(username) == null) {
            throw new RestResourceNotFoundException(String.format(EndpointMessages.NO_USER_FOUND, username));
        }
        userService.delete(username);
    }
}
