package com.pcalouche.spat.service;

import com.pcalouche.spat.ServiceTest;
import com.pcalouche.spat.dao.user.UserDao;
import com.pcalouche.spat.model.User;
import com.pcalouche.spat.service.user.UserService;
import com.pcalouche.spat.service.user.UserServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class UserServiceTest extends ServiceTest {
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

        given(userDao.getUsers()).willReturn(expectedUsers);

        assertThat(userService.getUsers()).isEqualTo(expectedUsers);

        verify(userDao, times(1)).getUsers();
    }

    @Test
    public void testSaveUser() {
        User expectedUser = new User(1L, "Philip", "Calouche");

        given(userDao.saveUser(expectedUser)).willReturn(expectedUser);

        assertThat(userService.saveUser(expectedUser)).isEqualToComparingFieldByField(expectedUser);

        verify(userDao, times(1)).saveUser(expectedUser);
    }

    @Test
    public void testDeleteUser() {
        given(userDao.deleteUser(1L)).willReturn(true);

        assertThat(userService.deleteUser(1L)).isTrue();
    }

}
