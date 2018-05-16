package com.pcalouche.spat.restservices;

import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.role.RoleRepository;
import com.pcalouche.spat.restservices.api.team.repository.TeamRepository;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
        List<Team> teams = new ArrayList<>();

        Team team1 = new Team();
        team1.setName("team1");
        teams.add(team1);

        Team team2 = new Team();
        team2.setName("team2");
        teams.add(team2);

        Team team3 = new Team();
        team3.setName("team3");
        teams.add(team3);

        teamRepository.saveAll(teams);

        Role userRole = roleRepository.save(new Role("ROLE_USER"));
        Role adminRole = roleRepository.save(new Role("ROLE_ADMIN"));

        List<User> users = new ArrayList<>();

        User user1 = new User();
        user1.setUsername("activeUser");
        user1.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));
        user1.setAccountNonExpired(true);
        user1.setCredentialsNonExpired(true);
        user1.setAccountNonLocked(true);
        user1.setEnabled(true);
        Set<Role> user1Roles = new HashSet<>();
        user1Roles.add(userRole);
        user1.setRoles(user1Roles);
        users.add(user1);

        User user2 = new User();
        user2.setUsername("activeAdmin");
        user2.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));
        user2.setAccountNonExpired(true);
        user2.setCredentialsNonExpired(true);
        user2.setAccountNonLocked(true);
        user2.setEnabled(true);
        Set<Role> user2Roles = new HashSet<>();
        user2Roles.add(userRole);
        user2Roles.add(adminRole);
        user2.setRoles(user2Roles);
        users.add(user2);

        User user3 = new User();
        user3.setUsername("expiredUser");
        user3.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));
        user3.setAccountNonExpired(false);
        user3.setCredentialsNonExpired(true);
        user3.setAccountNonLocked(true);
        user3.setEnabled(true);
        Set<Role> user3Roles = new HashSet<>();
        user3Roles.add(userRole);
        user3Roles.add(adminRole);
        user3.setRoles(user3Roles);
        users.add(user3);

        User user4 = new User();
        user4.setUsername("credentialsExpiredUser");
        user4.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));
        user4.setAccountNonExpired(true);
        user4.setCredentialsNonExpired(false);
        user4.setAccountNonLocked(true);
        user4.setEnabled(true);
        Set<Role> user4Roles = new HashSet<>();
        user4Roles.add(userRole);
        user4.setRoles(user4Roles);
        users.add(user4);

        User user5 = new User();
        user5.setUsername("lockedUser");
        user5.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));
        user5.setAccountNonExpired(true);
        user5.setCredentialsNonExpired(true);
        user5.setAccountNonLocked(false);
        user5.setEnabled(true);
        Set<Role> user5Roles = new HashSet<>();
        user5Roles.add(userRole);
        user5.setRoles(user5Roles);
        users.add(user5);

        User user6 = new User();
        user6.setUsername("disabledUser");
        user6.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));
        user6.setAccountNonExpired(true);
        user6.setCredentialsNonExpired(true);
        user6.setAccountNonLocked(true);
        user6.setEnabled(false);
        Set<Role> user6Roles = new HashSet<>();
        user6Roles.add(userRole);
        user6.setRoles(user6Roles);
        users.add(user6);

        userRepository.saveAll(users);
    }
}
