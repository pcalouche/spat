package com.pcalouche.spat.api.user;

import com.pcalouche.spat.AbstractServiceTest;
import com.pcalouche.spat.api.model.User;
import com.pcalouche.spat.api.user.dao.UserDao;
import com.pcalouche.spat.api.user.service.UserService;
import com.pcalouche.spat.api.user.service.UserServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

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

        assertThat(userService.getUsers()).isEqualTo(expectedUsers);

        Mockito.verify(userDao, Mockito.times(1)).getUsers();
    }

    @Test
    public void testSaveUser() {
        User expectedUser = new User(1L, "Philip", "Calouche");

        BDDMockito.given(userDao.saveUser(expectedUser)).willReturn(expectedUser);

        assertThat(userService.saveUser(expectedUser)).isEqualTo(expectedUser);

        Mockito.verify(userDao, Mockito.times(1)).saveUser(expectedUser);
    }

    @Test
    public void testDeleteUser() {
        BDDMockito.given(userDao.deleteUser(1L)).willReturn(true);

        assertThat(userService.deleteUser(1L)).isTrue();
    }

}
