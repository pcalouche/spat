package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.EndpointMessages;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.dto.UserEditRequest;
import com.pcalouche.spat.restservices.api.exception.RestResourceForbiddenException;
import com.pcalouche.spat.restservices.api.exception.RestResourceNotFoundException;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.service.UserService;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = ApiEndpoints.USERS)
public class UserController {
    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    @GetMapping(value = "/current-user")
    public UserDto currentUser(@AuthenticationPrincipal JwtAuthenticationToken jwtAuthenticationToken) {
        return userService.findByUsername(jwtAuthenticationToken.getPrincipal())
                .orElseThrow(() -> new RestResourceNotFoundException(String.format(EndpointMessages.NO_USER_FOUND, jwtAuthenticationToken.getPrincipal())));
    }

    @ApiOperation(value = "Find a user by username")
    @GetMapping(value = "/{id}")
    public UserDto findById(@PathVariable Integer id) {
        return userService.findById(id)
                .orElseThrow(() -> new RestResourceNotFoundException(String.format(EndpointMessages.NO_USER_FOUND, id)));
    }

    @ApiOperation(value = "Find all users")
    @GetMapping
    public List<UserDto> findAll() {
        return userService.findAll();
    }

    @ApiOperation(value = "Create a new user")
    @PreAuthorize("hasAuthority('Admin')")
    @PostMapping
    public UserDto create(@RequestBody UserEditRequest userEditRequest) {
        if (userService.findByUsername(userEditRequest.getUsername()).isPresent()) {
            throw new RestResourceForbiddenException(String.format(EndpointMessages.USER_ALREADY_EXISTS, userEditRequest.getUsername()));
        } else {
            return userService.create(userEditRequest);
        }
    }

    @ApiOperation(value = "Update an existing user")
    @PreAuthorize("hasAuthority('Admin')")
    @PutMapping(value = "/{id}")
    public UserDto update(@PathVariable Integer id, @RequestBody UserEditRequest userEditRequest) {
        return userService.update(id, userEditRequest)
                .orElseThrow(() -> new RestResourceNotFoundException(String.format(EndpointMessages.NO_USER_FOUND, id)));
    }

    @ApiOperation(value = "Delete an existing user")
    @PreAuthorize("hasAuthority('Admin')")
    @DeleteMapping(value = "/{id}")
    public void delete(@PathVariable Integer id) {
        if (userService.findById(id).isPresent()) {
            userService.delete(id);
        } else {
            throw new RestResourceNotFoundException(String.format(EndpointMessages.NO_USER_FOUND, id));
        }
    }
}
