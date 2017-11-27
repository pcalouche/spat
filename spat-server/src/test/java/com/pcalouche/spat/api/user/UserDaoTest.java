package com.pcalouche.spat.api.user;

import com.pcalouche.spat.AbstractDaoTest;
import com.pcalouche.spat.api.model.User;
import com.pcalouche.spat.api.user.dao.UserDao;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

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
        User newUser = new User(null, "FirstName", "LastName");

        User savedUser = userDao.saveUser(newUser);
        assertThat(savedUser).isEqualTo(newUser);

        savedUser.setFirstName("NewFirstName");

        User updatedUser = userDao.saveUser(savedUser);
        assertThat(updatedUser).isEqualTo(savedUser);
    }

    @Test
    public void testDeleteUser() {
        assertThat(userDao.deleteUser(1L)).isTrue();
        assertThat(userDao.deleteUser(-1L)).isFalse();
    }
}
