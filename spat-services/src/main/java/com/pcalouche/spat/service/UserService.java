package com.pcalouche.spat.service;

import com.pcalouche.spat.api.dto.UserDto;
import com.pcalouche.spat.api.dto.UserEditRequest;

import java.util.List;
import java.util.Optional;

public interface UserService {
    Optional<UserDto> findById(Integer id);

    Optional<UserDto> findByUsername(String username);

    List<UserDto> findAll();

    UserDto create(UserEditRequest userEditRequest);

    Optional<UserDto> update(Integer id, UserEditRequest userEditRequest);

    void delete(Integer id);
}
