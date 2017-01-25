package com.calouche.spat.dao;

import com.calouche.spat.IntegratedTest;
import com.calouche.spat.dao.user.UserDao;
import com.calouche.spat.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class UserDaoTest extends IntegratedTest {
    @Autowired
    private UserDao userDao;

    @Test
    void getUsersTest() {
        List<User> users = userDao.getUsers();
        Assert.assertNotNull(users);
    }

    @Test
    void saveUserTest() {
        User newUser = new User(null, "FirstName", "LastName");

        User savedUser = userDao.saveUser(newUser);
        Assert.assertEquals(savedUser, newUser);

        savedUser.setFirstName("NewFirstName");

        User updatedUser = userDao.saveUser(savedUser);
        Assert.assertEquals(updatedUser, savedUser);
    }

    @Test
    void deleteUserTest() {
        Assert.assertTrue(userDao.deleteUser(1L));
        Assert.assertFalse(userDao.deleteUser(-1L));
    }
}
