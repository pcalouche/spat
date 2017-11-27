package com.pcalouche.spat.security.provider;

import com.pcalouche.spat.api.model.User;
import com.pcalouche.spat.api.user.service.UserService;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

@Component
public class AjaxLoginAuthenticationProvider implements AuthenticationProvider {
    private final UserService userService;
    private final PasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    public AjaxLoginAuthenticationProvider(UserService userService) {
        this.userService = userService;
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        String username = (String) authentication.getPrincipal();
        String password = (String) authentication.getCredentials();

        User user = userService.getByUsername(username);
        if (user != null && passwordEncoder.matches(password, user.getPassword())) {
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