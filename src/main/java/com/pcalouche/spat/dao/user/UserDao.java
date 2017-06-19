package com.pcalouche.spat.dao.user;

import com.pcalouche.spat.model.User;

import java.util.List;

public interface UserDao {
    List<User> getUsers();

    User saveUser(User user);

    Boolean deleteUser(Long id);
}
