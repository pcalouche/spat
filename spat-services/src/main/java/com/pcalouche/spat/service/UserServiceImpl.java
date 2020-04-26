package com.pcalouche.spat.service;

import com.pcalouche.spat.api.dto.RoleDto;
import com.pcalouche.spat.api.dto.UserDto;
import com.pcalouche.spat.api.dto.UserEditRequest;
import com.pcalouche.spat.entity.User;
import com.pcalouche.spat.repository.RoleRepository;
import com.pcalouche.spat.repository.UserRepository;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Sort;
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
    @Transactional(readOnly = true)
    public Optional<UserDto> findById(Integer id) {
        return userRepository.findById(id)
                .map(user -> modelMapper.map(user, UserDto.class));
    }

    @Override
    @Transactional(readOnly = true)
    public Optional<UserDto> findByUsername(String username) {
        return userRepository.findByUsername(username)
                .map(user -> modelMapper.map(user, UserDto.class));
    }

    @Override
    @Transactional(readOnly = true)
    public List<UserDto> findAll() {
        return userRepository.findAll(Sort.by("username")).stream()
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
            roleRepository.findByName(roleDto.getName())
                    .ifPresent(role -> user.getRoles().add(role));
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
            user.setUsername(userEditRequest.getUsername());
            user.setAccountNonExpired(userEditRequest.isAccountNonExpired());
            user.setAccountNonLocked(userEditRequest.isAccountNonLocked());
            user.setCredentialsNonExpired(userEditRequest.isCredentialsNonExpired());
            user.setEnabled(userEditRequest.isEnabled());

            user.getRoles().clear();

            for (RoleDto roleDto : userEditRequest.getRoleDtos()) {
                roleRepository.findByName(roleDto.getName())
                        .ifPresent(role -> user.getRoles().add(role));
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