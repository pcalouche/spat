package com.pcalouche.spat.restservices.api.user;

import com.pcalouche.spat.restservices.AbstractServiceTest;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.dao.UserDao;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import com.pcalouche.spat.restservices.api.user.service.UserServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

public class UserServiceTest extends AbstractServiceTest {
    @MockBean
    private UserDao userDao;
    @MockBean
    private UserRepository userRepository;
    private UserService userService;

    @Before
    public void before() {
        userService = new UserServiceImpl(userDao, userRepository);
    }

    @Test
    public void testGetUsers() {
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
        List<User> expectedUsers = new ArrayList<>();
        expectedUsers.add(new User(1L, "pcalouche", authorities));
        expectedUsers.add(new User(2L, "jsmith", authorities));

        given(userDao.getUsers()).willReturn(expectedUsers);

        assertThat(userService.getUsers()).isEqualTo(expectedUsers);

        verify(userDao, Mockito.times(1)).getUsers();
    }

    @Test
    public void testGetByUsername() {
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
        User expectedUser = new User(1L, "pcalouche", authorities);

        given(userDao.getByUsername(expectedUser.getUsername())).willReturn(expectedUser);

        assertThat(userService.getByUsername(expectedUser.getUsername())).isEqualTo(expectedUser);

        verify(userDao, Mockito.times(1)).getByUsername(expectedUser.getUsername());
    }

    @Test
    public void testSaveUser() {
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
        User expectedUser = new User(1L, "pcalouche", authorities);

        given(userDao.saveUser(expectedUser)).willReturn(expectedUser);

        assertThat(userService.saveUser(expectedUser)).isEqualTo(expectedUser);

        verify(userDao, Mockito.times(1)).saveUser(expectedUser);
    }

    @Test
    public void testDeleteUser() {
        given(userDao.deleteUser(1L)).willReturn(true);

        assertThat(userService.deleteUser(1L)).isTrue();
    }
}
