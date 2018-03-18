package com.pcalouche.spat.restservices.api.user.service;

import com.pcalouche.spat.restservices.api.entity.User;

import java.util.List;

public interface UserService {
    User getByUsername(String username);

    List<User> getUsers();

    User saveUser(User user);

    Boolean deleteUser(Long id);
}
