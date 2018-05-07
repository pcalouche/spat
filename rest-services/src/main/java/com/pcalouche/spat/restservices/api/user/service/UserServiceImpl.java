package com.pcalouche.spat.restservices.api.user.service;

import com.pcalouche.spat.restservices.api.AbstractSpatServiceImpl;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.dao.UserDao;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class UserServiceImpl extends AbstractSpatServiceImpl implements UserService {
    private final UserDao userDao;
    private final UserRepository userRepository;

    public UserServiceImpl(UserDao userDao, UserRepository userRepository) {
        this.userDao = userDao;
        this.userRepository = userRepository;
    }

    @Override
    public User getByUsername(String username) {
        return userRepository.findByUsername(username);
    }

    @Override
    public List<User> getUsers() {
        List<User> users = new ArrayList<>();
        userRepository.findAll().forEach(users::add);
        return users;
    }

    @Override
    public User saveUser(User user) {
        if (user.getId() == null && userDao.getByUsername(user.getUsername()) != null) {
            throw new IllegalArgumentException(String.format("A user with a username of %s already exists", user.getUsername()));
        }
        return userDao.saveUser(user);
    }

    @Override
    public Boolean deleteUser(Long id) {
        userRepository.deleteById(id);
        return true;
    }
}