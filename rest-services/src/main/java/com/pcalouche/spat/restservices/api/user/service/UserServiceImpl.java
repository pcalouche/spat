package com.pcalouche.spat.restservices.api.user.service;

import com.pcalouche.spat.restservices.api.AbstractServiceImpl;
import com.pcalouche.spat.restservices.api.model.User;
import com.pcalouche.spat.restservices.api.user.dao.UserDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class UserServiceImpl extends AbstractServiceImpl implements UserService {
    private final UserDao userDao;

    @Autowired
    public UserServiceImpl(UserDao userDao) {
        this.userDao = userDao;
    }

    @Override
    public User getByUsername(String username) {
        return userDao.getByUsername(username);
    }

    @Override
    public List<User> getUsers() {
        return userDao.getUsers();
    }

    @Override
    public User saveUser(User user) {
        return userDao.saveUser(user);
    }

    @Override
    public Boolean deleteUser(Long id) {
        return userDao.deleteUser(id);
    }
}