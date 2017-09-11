package com.pcalouche.spat.service;

import com.pcalouche.spat.AbstractTest;
import com.pcalouche.spat.dao.user.UserDao;
import org.springframework.boot.test.mock.mockito.MockBean;

public class UserServiceTest extends AbstractTest {
    @MockBean
    private UserDao userDao;
}
