package com.pcalouche.spat.restservices.api.service;

import com.pcalouche.spat.restservices.api.dto.UserDto;

import java.util.List;

public interface UserService {
    UserDto findById(String username);

    List<UserDto> findAll();

    UserDto save(UserDto userDto);

    void delete(String username);
}
