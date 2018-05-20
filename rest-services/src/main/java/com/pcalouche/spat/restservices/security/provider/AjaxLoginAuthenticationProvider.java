package com.pcalouche.spat.restservices.security.provider;

import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.stereotype.Component;

@Component
public class AjaxLoginAuthenticationProvider implements AuthenticationProvider {
    private final UserRepository userRepository;

    public AjaxLoginAuthenticationProvider(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        String username = (String) authentication.getPrincipal();
        String password = (String) authentication.getCredentials();

        User user = userRepository.findByUsername(username);
        if (user != null && SecurityUtils.PASSWORD_ENCODER.matches(password, user.getPassword())) {
            SecurityUtils.validateUserAccountStatus(user);
        } else {
            throw new BadCredentialsException(String.format("Bad credentials for username: %s", username));
        }

        // If we make it here authentication was good, so return a good authentication without credentials for the request to proceed.
        return new UsernamePasswordAuthenticationToken(username, null, user.getAuthorities());
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return (UsernamePasswordAuthenticationToken.class.isAssignableFrom(authentication));
    }
}