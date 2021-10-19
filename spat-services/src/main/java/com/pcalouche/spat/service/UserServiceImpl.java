package com.pcalouche.spat.service;

import com.pcalouche.spat.api.dto.RoleDto;
import com.pcalouche.spat.api.dto.UserDto;
import com.pcalouche.spat.api.dto.UserEditRequest;
import com.pcalouche.spat.entity.User;
import com.pcalouche.spat.repository.RoleRepository;
import com.pcalouche.spat.repository.UserRepository;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class UserServiceImpl implements UserService {
    private final UserRepository userRepository;
    private final RoleRepository roleRepository;

    public UserServiceImpl(UserRepository userRepository, RoleRepository roleRepository) {
        this.userRepository = userRepository;
        this.roleRepository = roleRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public Optional<UserDto> findById(Integer id) {
        return userRepository.findById(id)
                .map(UserDto::map);
    }

    @Override
    @Transactional(readOnly = true)
    public Optional<UserDto> findByUsername(String username) {
        return userRepository.findByUsername(username)
                .map(UserDto::map);
    }

    @Override
    @Transactional(readOnly = true)
    public List<UserDto> findAll() {
        return userRepository.findAll(Sort.by("username")).stream()
                .map(UserDto::map)
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

        userEditRequest.getRoleDtos().forEach(roleDto ->
                roleRepository.findByName(roleDto.getName())
                        .ifPresent(role -> user.getRoles().add(role))
        );

        return UserDto.map(userRepository.save(user));
    }

    @Override
    @Transactional
    public Optional<UserDto> update(Integer id, UserEditRequest userEditRequest) {
        return userRepository.findById(id)
                .map(user -> {
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

                    return UserDto.map(userRepository.save(user));
                });
    }

    @Override
    @Transactional
    public void delete(Integer id) {
        userRepository.deleteById(id);
    }
}