package com.pcalouche.spat.restservices.api.user.service;

import com.pcalouche.spat.restservices.api.AbstractSpatServiceImpl;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class UserServiceImpl extends AbstractSpatServiceImpl implements UserService {
    private final UserRepository userRepository;
    private final ModelMapper modelMapper;

    public UserServiceImpl(UserRepository userRepository, ModelMapper modelMapper) {
        this.userRepository = userRepository;
        this.modelMapper = modelMapper;
    }

    @Override
    public UserDto findByUsername(String username) {
        User user = userRepository.findByUsername(username);
        return modelMapper.map(user, UserDto.class);
    }

    @Override
    public List<UserDto> findAll() {
        List<UserDto> userDtos = new ArrayList<>();
        userRepository.findAll().forEach(user -> userDtos.add(modelMapper.map(user, UserDto.class)));
        return userDtos;
    }

    @Override
    public UserDto save(UserDto userDto) {
        // Don't allow saves of duplicate usernames.
        if (userDto.getId() == null && userRepository.findByUsername(userDto.getUsername()) != null) {
            throw new IllegalArgumentException(String.format("A user with a username of %s already exists", userDto.getUsername()));
        }

        User userToSave = modelMapper.map(userDto, User.class);
        // Retain password if saving an existing user
        Optional<User> existingUser;
        if (userDto.getId() != null) {
            existingUser = userRepository.findById(userDto.getId());
            existingUser.ifPresent(user1 -> userToSave.setPassword(user1.getPassword()));
        }

        // Set default password if blank still
        if (StringUtils.isEmpty(userToSave.getPassword())) {
            userToSave.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));
        }

        return modelMapper.map(userRepository.save(userToSave), UserDto.class);
    }

    @Override
    public void deleteById(Long id) {
        userRepository.deleteById(id);
    }
}