package com.pcalouche.spat.restservices.security.authentication;

import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;

import java.util.Collection;

public class JwtAuthenticationToken extends AbstractAuthenticationToken {
    private final String subject;
    private final String token;

    public JwtAuthenticationToken(String token) {
        super(null);
        this.token = token;
        subject = null;
        setAuthenticated(false);
    }

    public JwtAuthenticationToken(String subject,
                                  String token,
                                  Collection<? extends GrantedAuthority> authorities) {
        super(authorities);
        this.subject = subject;
        this.token = token;
        setAuthenticated(true);
    }

    @Override
    public Object getPrincipal() {
        return subject;
    }

    @Override
    public Object getCredentials() {
        return token;
    }
}
