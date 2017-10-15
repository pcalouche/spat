package com.pcalouche.spat.security;

import com.pcalouche.spat.model.SpatUser;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
public class SpatUserDetailsService implements UserDetailsService {
    //    private ApplicationUserRepository applicationUserRepository;
    //
    //    public UserDetailsServiceImpl(ApplicationUserRepository applicationUserRepository) {
    //        this.applicationUserRepository = applicationUserRepository;
    //    }
    //
    //    @Override
    //    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
    //        ApplicationUser applicationUser = applicationUserRepository.findByUsername(username);
    //        if (applicationUser == null) {
    //            throw new UsernameNotFoundException(username);
    //        }
    //        return new User(applicationUser.getUsername(), applicationUser.getPassword(), emptyList());
    //    }

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        SpatUser spatUser = null;

        if ("user" .equals(username)) {
            spatUser = new SpatUser();
            spatUser.setId(1L);
            spatUser.setUsername("user");
            spatUser.setPassword("password");
            spatUser.setRole("USER");
        }

        if (spatUser == null) {
            throw new UsernameNotFoundException(username);
        }

        return new SpatUserDetails(spatUser);
    }
}
