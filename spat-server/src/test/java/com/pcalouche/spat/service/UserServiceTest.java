package com.pcalouche.spat.service;

import com.pcalouche.spat.AbstractServiceTest;
import com.pcalouche.spat.dao.user.UserDao;
import com.pcalouche.spat.model.User;
import com.pcalouche.spat.service.user.UserService;
import com.pcalouche.spat.service.user.UserServiceImpl;
import org.assertj.core.api.Assertions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

public class UserServiceTest extends AbstractServiceTest {
    @MockBean
    private UserDao userDao;
    private UserService userService;

    @Before
    public void before() {
        userService = new UserServiceImpl(userDao);
    }

    @Test
    public void testGetUsers() {
        List<User> expectedUsers = new ArrayList<>();
        expectedUsers.add(new User(1L, "Philip", "Calouche"));
        expectedUsers.add(new User(2L, "Joe", "Smith"));

        BDDMockito.given(userDao.getUsers()).willReturn(expectedUsers);

        Assertions.assertThat(userService.getUsers()).isEqualTo(expectedUsers);

        Mockito.verify(userDao, Mockito.times(1)).getUsers();
    }

    @Test
    public void testSaveUser() {
        User expectedUser = new User(1L, "Philip", "Calouche");

        BDDMockito.given(userDao.saveUser(expectedUser)).willReturn(expectedUser);

        Assertions.assertThat(userService.saveUser(expectedUser)).isEqualTo(expectedUser);

        Mockito.verify(userDao, Mockito.times(1)).saveUser(expectedUser);
    }

    @Test
    public void testDeleteUser() {
        BDDMockito.given(userDao.deleteUser(1L)).willReturn(true);

        Assertions.assertThat(userService.deleteUser(1L)).isTrue();
    }

}
