package com.calouche.spat.service;

import com.calouche.spat.dao.user.UserDao;
import com.calouche.spat.model.User;
import com.calouche.spat.service.user.UserService;
import com.calouche.spat.service.user.UserServiceImpl;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

public class UserServiceTest {
    private final UserDao userDao = Mockito.mock(UserDao.class);
    private final UserService userService = new UserServiceImpl(userDao);

    @Test
    public void getUsersTest() {
        List<User> expectedUsers = new ArrayList<>();
        expectedUsers.add(new User(1L, "Philip", "Calouche"));
        expectedUsers.add(new User(2L, "Joe", "Smith"));

        Mockito.when(userDao.getUsers()).thenReturn(expectedUsers);

        List<User> actualUsers = userService.getUsers();
        Assert.assertEquals(actualUsers.size(), expectedUsers.size());
        for (int i = 0; i < actualUsers.size(); i++) {
            Assert.assertEquals(actualUsers.get(i), expectedUsers.get(i));
        }
    }

    @Test
    void saveUserTest() {
        User expectedUser = new User(1L, "Philip", "Calouche");

        Mockito.when(userDao.saveUser(expectedUser)).thenReturn(expectedUser);

        User actualUser = userService.saveUser(expectedUser);
        Assert.assertEquals(actualUser, expectedUser);
    }

    @Test
    void deleteUserTest() {
        Mockito.when(userDao.deleteUser(1L)).thenReturn(true);
        Assert.assertTrue(userService.deleteUser(1L));
    }
}
