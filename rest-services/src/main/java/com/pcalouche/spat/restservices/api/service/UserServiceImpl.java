package com.pcalouche.spat.restservices.api.service;

import com.pcalouche.spat.restservices.api.AbstractSpatServiceImpl;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.repository.RoleRepository;
import com.pcalouche.spat.restservices.api.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
public class UserServiceImpl extends AbstractSpatServiceImpl implements UserService {
    private final ModelMapper modelMapper;
    private final UserRepository userRepository;
    private final RoleRepository roleRepository;

    public UserServiceImpl(ModelMapper modelMapper, UserRepository userRepository, RoleRepository roleRepository) {
        this.modelMapper = modelMapper;
        this.userRepository = userRepository;
        this.roleRepository = roleRepository;
    }

    @Override
    public UserDto findById(String username) {
        return userRepository.findById(username)
                .map(user -> modelMapper.map(user, UserDto.class))
                .orElse(null);
    }

    @Override
    public List<UserDto> findAll() {
        List<UserDto> userDtos = new ArrayList<>();
        userRepository.findAll().forEach(user -> userDtos.add(modelMapper.map(user, UserDto.class)));
        return userDtos;
    }

    @Override
    public UserDto save(UserDto userDto) {
        User userToSave = modelMapper.map(userDto, User.class);
        Optional<User> existingUser = userRepository.findById(userDto.getUsername());
        if (existingUser.isPresent()) {
            // Retain password
            userToSave.setPassword(existingUser.get().getPassword());
        } else {
            // Set default password
            userToSave.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));
        }

        // Save with at least one role
        if (userToSave.getRoles().isEmpty()) {
            userToSave.setRoles(Stream.of(roleRepository.findByName("ROLE_USER"))
                    .collect(Collectors.toSet()));
        }

        return modelMapper.map(userRepository.save(userToSave), UserDto.class);
    }

    @Override
    public void delete(String username) {
        userRepository.deleteById(username);
    }
}