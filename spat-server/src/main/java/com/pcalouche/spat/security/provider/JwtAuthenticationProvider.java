package com.pcalouche.spat.security.provider;

import com.pcalouche.spat.api.model.User;
import com.pcalouche.spat.api.user.dao.UserDao;
import com.pcalouche.spat.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.security.util.SecurityUtils;
import io.jsonwebtoken.Claims;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class JwtAuthenticationProvider implements AuthenticationProvider {
    private final UserDao userDao;

    public JwtAuthenticationProvider(UserDao userDao) {
        this.userDao = userDao;
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        // Try to validate the provided authentication as JwtAuthentication
        JwtAuthenticationToken jwtAuthentication = (JwtAuthenticationToken) authentication;
        if (jwtAuthentication.getCredentials() == null) {
            throw new BadCredentialsException("No credentials found in JWT.  Was a JWT provided in the Authorization header?");
        }
        String token = jwtAuthentication.getCredentials().toString();
        Claims claims = SecurityUtils.getClaimsFromToken(token);
        String subject = claims.getSubject();

        // When a refresh token happens double check the account status and the authorities with what is in the database to ensure the account is still active.
        List<SimpleGrantedAuthority> simpleGrantedAuthorities;
        if ("refreshToken".equals(authentication.getDetails())) {
            User user = userDao.getByUsername(subject);
            simpleGrantedAuthorities = user.getAuthorities();
        } else {
            @SuppressWarnings("unchecked")
            List<String> authorities = claims.get(SecurityUtils.CLAIMS_AUTHORITIES_KEY, List.class);
            simpleGrantedAuthorities = authorities.stream()
                    .map(SimpleGrantedAuthority::new)
                    .collect(Collectors.toList());
        }

        // Authentication was good, so return a good authentication without credentials so the request can continue
        return new JwtAuthenticationToken(subject, null, simpleGrantedAuthorities);
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return (JwtAuthenticationToken.class.isAssignableFrom(authentication));
    }
}
