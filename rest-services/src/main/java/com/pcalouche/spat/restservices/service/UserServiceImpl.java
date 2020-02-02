package com.pcalouche.spat.restservices.service;

import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.dto.UserEditRequest;
import com.pcalouche.spat.restservices.entity.User;
import com.pcalouche.spat.restservices.repository.RoleRepository;
import com.pcalouche.spat.restservices.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class UserServiceImpl implements UserService {
    private final ModelMapper modelMapper;
    private final UserRepository userRepository;
    private final RoleRepository roleRepository;

    public UserServiceImpl(ModelMapper modelMapper, UserRepository userRepository, RoleRepository roleRepository) {
        this.modelMapper = modelMapper;
        this.userRepository = userRepository;
        this.roleRepository = roleRepository;
    }

    @Override
    @Transactional
    public Optional<UserDto> findById(Integer id) {
        Optional<UserDto> userDtoOptional = Optional.empty();
        Optional<User> userOptional = userRepository.findById(id);
        if (userOptional.isPresent()) {
            userDtoOptional = Optional.of(modelMapper.map(userOptional.get(), UserDto.class));
        }
        return userDtoOptional;
    }

    @Override
    public Optional<UserDto> findByUsername(String username) {
        Optional<UserDto> userDtoOptional = Optional.empty();
        Optional<User> userOptional = userRepository.findByUsername(username);
        if (userOptional.isPresent()) {
            userDtoOptional = Optional.of(modelMapper.map(userOptional.get(), UserDto.class));
        }
        return userDtoOptional;
    }

    @Override
    @Transactional
    public List<UserDto> findAll() {
        return userRepository.findAll().stream()
                .map(user -> modelMapper.map(user, UserDto.class))
                .collect(Collectors.toList());
    }

    @Override
    @Transactional
    public UserDto create(UserEditRequest userEditRequest) {
        User user = User.builder()
                .username(userEditRequest.getUsername())
                // Set default password. In real application something like an email with a temp password would be sent instead.
                .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                .accountNonExpired(userEditRequest.isAccountNonExpired())
                .accountNonLocked(userEditRequest.isAccountNonLocked())
                .credentialsNonExpired(userEditRequest.isCredentialsNonExpired())
                .enabled(userEditRequest.isEnabled())
                .build();

        for (RoleDto roleDto : userEditRequest.getRoleDtos()) {
            user.getRoles().add(roleRepository.findByName(roleDto.getName()));
        }

        return modelMapper.map(userRepository.save(user), UserDto.class);
    }

    @Override
    @Transactional
    public Optional<UserDto> update(Integer id, UserEditRequest userEditRequest) {
        Optional<UserDto> userDtoOptional = Optional.empty();
        Optional<User> userOptional = userRepository.findById(id);
        if (userOptional.isPresent()) {
            User user = userOptional.get();
            // TODO be able to update user name
            user.setAccountNonExpired(userEditRequest.isAccountNonExpired());
            user.setAccountNonLocked(userEditRequest.isAccountNonLocked());
            user.setCredentialsNonExpired(userEditRequest.isCredentialsNonExpired());
            user.setEnabled(userEditRequest.isEnabled());

            user.getRoles().clear();
            for (RoleDto roleDto : userEditRequest.getRoleDtos()) {
                user.getRoles().add(roleRepository.findByName(roleDto.getName()));
            }

            userDtoOptional = Optional.of(modelMapper.map(userRepository.save(user), UserDto.class));
        }

        return userDtoOptional;
    }

    @Override
    @Transactional
    public void delete(Integer id) {
        userRepository.deleteById(id);
    }
}