package com.pcalouche.spat.service.user;

import com.pcalouche.spat.dao.user.UserDao;
import com.pcalouche.spat.model.User;
import com.pcalouche.spat.service.AbstractServiceImpl;
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