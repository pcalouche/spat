package com.pcalouche.spat.restservices.security.provider;

import com.pcalouche.spat.restservices.entity.User;
import com.pcalouche.spat.restservices.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

import java.util.Optional;

public class LoginAuthenticationProvider implements AuthenticationProvider {
    private final UserRepository userRepository;

    public LoginAuthenticationProvider(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        String username = (String) authentication.getPrincipal();
        String password = (String) authentication.getCredentials();

        Optional<User> optionalUser = userRepository.findByUsername(username);
        if (!optionalUser.isPresent() || !SecurityUtils.PASSWORD_ENCODER.matches(password, optionalUser.get().getPassword())) {
            throw new BadCredentialsException(String.format("Bad credentials for username: %s", username));
        } else {
            User user = optionalUser.get();
            SecurityUtils.validateUserAccountStatus(user);
            return new UsernamePasswordAuthenticationToken(username, null, user.getAuthorities());
        }
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return (UsernamePasswordAuthenticationToken.class.isAssignableFrom(authentication));
    }
}