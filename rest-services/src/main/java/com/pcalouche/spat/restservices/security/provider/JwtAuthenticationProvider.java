package com.pcalouche.spat.restservices.security.provider;

import com.pcalouche.spat.restservices.entity.User;
import com.pcalouche.spat.restservices.repository.UserRepository;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class JwtAuthenticationProvider implements AuthenticationProvider {
    private final UserRepository userRepository;

    public JwtAuthenticationProvider(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        // Try to validate the provided authentication as JwtAuthenticationToken
        JwtAuthenticationToken jwtAuthenticationToken = (JwtAuthenticationToken) authentication;
        if (jwtAuthenticationToken.getCredentials() == null) {
            throw new BadCredentialsException("JSON web token was empty.");
        }
        String token = jwtAuthenticationToken.getCredentials().toString();
        Claims claims;
        try {
            claims = SecurityUtils.getClaimsFromToken(token);
        } catch (JwtException e) {
            throw new BadCredentialsException("JSON web token was invalid");
        }
        String subject = claims.getSubject();

        // When a refresh token happens double check the account status and the authorities
        // with what is in the database to ensure the account is still active.
        Set<SimpleGrantedAuthority> simpleGrantedAuthorities;
        if (Boolean.valueOf(claims.get(SecurityUtils.CLAIMS_REFRESH_TOKEN_KEY).toString())) {
            Optional<User> optionalUser = userRepository.findById(subject);
            if (optionalUser.isPresent()) {
                SecurityUtils.validateUserAccountStatus(optionalUser.get());
            } else {
                throw new BadCredentialsException(String.format("Bad credentials for username: %s", subject));
            }

            simpleGrantedAuthorities = optionalUser.get().getAuthorities();
        } else {
            @SuppressWarnings("unchecked")
            Set<String> authorities = new HashSet<>(claims.get(SecurityUtils.CLAIMS_AUTHORITIES_KEY, List.class));
            simpleGrantedAuthorities = authorities.stream()
                    .map(SimpleGrantedAuthority::new)
                    .collect(Collectors.toSet());
        }

        // Authentication was good, so return a good authentication without credentials so the request can continue
        return new JwtAuthenticationToken(subject, null, simpleGrantedAuthorities);
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return (JwtAuthenticationToken.class.isAssignableFrom(authentication));
    }
}
