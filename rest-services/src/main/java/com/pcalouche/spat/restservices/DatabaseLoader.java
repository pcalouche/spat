package com.pcalouche.spat.restservices;

import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.repository.RoleRepository;
import com.pcalouche.spat.restservices.api.repository.TeamRepository;
import com.pcalouche.spat.restservices.api.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component
public class DatabaseLoader implements CommandLineRunner {
    private final UserRepository userRepository;
    private final TeamRepository teamRepository;
    private final RoleRepository roleRepository;

    public DatabaseLoader(UserRepository userRepository, TeamRepository teamRepository, RoleRepository roleRepository) {
        this.userRepository = userRepository;
        this.teamRepository = teamRepository;
        this.roleRepository = roleRepository;
    }

    @Override
    public void run(String... args) {
        List<Team> teams = Arrays.asList(
                Team.builder()
                        .name("NC State Wolfpack")
                        .build(),
                Team.builder()
                        .name("Carolina Panthers")
                        .build(),
                Team.builder()
                        .name("Charlotte Hornets")
                        .build(),
                Team.builder()
                        .name("Carolina Hurricanes")
                        .build(),
                Team.builder()
                        .name("Charlotte Knights")
                        .build());

        teamRepository.saveAll(teams);

        Role userRole = roleRepository.save(Role.builder()
                .name("ROLE_USER")
                .build());
        Role adminRole = roleRepository.save(Role.builder()
                .name("ROLE_ADMIN")
                .build());

        List<User> users = Arrays.asList(
                User.builder()
                        .username("activeUser")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(userRole).collect(Collectors.toSet()))
                        .build(),
                User.builder()
                        .username("activeAdmin")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(userRole, adminRole)
                                .collect(Collectors.toSet()))
                        .build(),
                User.builder()
                        .username("expiredUser")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(userRole, adminRole)
                                .collect(Collectors.toSet()))
                        .accountNonExpired(false)
                        .build(),
                User.builder()
                        .username("credentialsExpiredUser")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(userRole).collect(Collectors.toSet()))
                        .credentialsNonExpired(false)
                        .build(),
                User.builder()
                        .username("lockedUser")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(userRole).collect(Collectors.toSet()))
                        .accountNonLocked(false)
                        .build(),
                User.builder()
                        .username("disabledUser")
                        .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                        .roles(Stream.of(userRole).collect(Collectors.toSet()))
                        .enabled(false)
                        .build()
        );

        userRepository.saveAll(users);
    }
}
