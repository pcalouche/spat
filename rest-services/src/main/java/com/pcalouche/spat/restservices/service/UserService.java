package com.pcalouche.spat.restservices.service;

import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.dto.UserEditRequest;

import java.util.List;
import java.util.Optional;

public interface UserService {
    Optional<UserDto> findById(String username);

    List<UserDto> findAll();

    UserDto create(UserEditRequest userEditRequest);

    Optional<UserDto> update(String username, UserEditRequest userEditRequest);

    void delete(String username);
}
