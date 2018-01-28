package com.pcalouche.spat.restservices.api.user;

import com.pcalouche.spat.restservices.AbstractDaoTest;
import com.pcalouche.spat.restservices.api.model.User;
import com.pcalouche.spat.restservices.api.user.dao.UserDao;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

public class UserDaoTest extends AbstractDaoTest {
    @Autowired
    private UserDao userDao;

    @Test
    public void testGetUsers() {
        List<User> users = userDao.getUsers();
        assertThat(users).isNotNull();
    }

    @Test
    public void testSaveUser() {
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
        User newUser = new User(null, "pcalouche", authorities);
        newUser.setPassword("password");

        User savedUser = userDao.saveUser(newUser);
        assertThat(savedUser).isEqualTo(newUser);

        savedUser.setUsername("pcalouche2");

        User updatedUser = userDao.saveUser(savedUser);
        assertThat(updatedUser).isEqualTo(savedUser);
    }

    @Test
    public void testDeleteUser() {
        assertThat(userDao.deleteUser(1L)).isTrue();
        assertThat(userDao.deleteUser(-1L)).isFalse();
    }
}
