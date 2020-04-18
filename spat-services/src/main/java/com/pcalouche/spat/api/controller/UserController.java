package com.pcalouche.spat.api.controller;

import com.pcalouche.spat.api.EndpointMessages;
import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.api.dto.UserDto;
import com.pcalouche.spat.api.dto.UserEditRequest;
import com.pcalouche.spat.api.exception.RestResourceForbiddenException;
import com.pcalouche.spat.api.exception.RestResourceNotFoundException;
import com.pcalouche.spat.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.service.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "User endpoints")
@RestController
@RequestMapping(value = Endpoints.USERS)
public class UserController {
    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    @GetMapping(value = "/current-user")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "get current user"),
            @ApiResponse(responseCode = "404", description = "current user no longer exists")
    })
    public UserDto currentUser(@AuthenticationPrincipal JwtAuthenticationToken jwtAuthenticationToken) {
        return userService.findByUsername(jwtAuthenticationToken.getPrincipal())
                .orElseThrow(() -> new RestResourceNotFoundException(EndpointMessages.CURRENT_USER_NOT_FOUND));
    }

    @Operation(description = "Find all users")
    @GetMapping
    public List<UserDto> findAll() {
        return userService.findAll();
    }

    @Operation(description = "Create a new user")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "user created"),
            @ApiResponse(responseCode = "403", description = "user already exists")
    })
    @PreAuthorize("hasAuthority('Admin')")
    @PostMapping
    public UserDto create(@RequestBody @Validated UserEditRequest userEditRequest) {
        if (userService.findByUsername(userEditRequest.getUsername()).isPresent()) {
            throw new RestResourceForbiddenException(String.format(EndpointMessages.USER_ALREADY_EXISTS, userEditRequest.getUsername()));
        } else {
            return userService.create(userEditRequest);
        }
    }

    @Operation(description = "Update an existing user")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "user update"),
            @ApiResponse(responseCode = "404", description = "user not found")
    })
    @PreAuthorize("hasAuthority('Admin')")
    @PutMapping(value = "/{id}")
    public UserDto update(@PathVariable Integer id, @RequestBody @Validated UserEditRequest userEditRequest) {
        return userService.update(id, userEditRequest)
                .orElseThrow(() -> new RestResourceNotFoundException(String.format(EndpointMessages.NO_USER_FOUND, id)));
    }

    @Operation(description = "Delete an existing user")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "user deleted"),
            @ApiResponse(responseCode = "404", description = "user not found")
    })
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