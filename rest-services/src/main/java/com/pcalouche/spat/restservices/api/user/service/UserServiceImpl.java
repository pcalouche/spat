package com.pcalouche.spat.restservices.api.user.service;

import com.pcalouche.spat.restservices.api.AbstractSpatServiceImpl;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class UserServiceImpl extends AbstractSpatServiceImpl implements UserService {
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    public UserServiceImpl(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public User getByUsername(String username) {
        return userRepository.findByUsername(username);
    }

    @Override
    public List<User> getUsers() {
        List<User> users = new ArrayList<>();
        userRepository.findAll().forEach(users::add);
        return users;
    }

    @Override
    public User saveUser(User user) {
        // Don't allow saves of duplicate usernames.
        if (user.getId() == null && userRepository.findByUsername(user.getUsername()) != null) {
            throw new IllegalArgumentException(String.format("A user with a username of %s already exists", user.getUsername()));
        }

        // Retain password if saving an existing user
        Optional<User> existingUser;
        if (user.getId() != null) {
            existingUser = userRepository.findById(user.getId());
            existingUser.ifPresent(user1 -> user.setPassword(user1.getPassword()));
        }

        // Set default password if blank still
        if (StringUtils.isEmpty(user.getPassword())) {
            user.setPassword(passwordEncoder.encode("password"));
        }

        return userRepository.save(user);
    }

    @Override
    public Boolean deleteUser(Long id) {
        userRepository.deleteById(id);
        return true;
    }
}