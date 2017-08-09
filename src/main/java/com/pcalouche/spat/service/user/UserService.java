package com.pcalouche.spat.service.user;

import com.pcalouche.spat.model.User;

import java.util.List;

public interface UserService {
    List<User> getUsers();

    User saveUser(User user);

    Boolean deleteUser(Long id);
}
