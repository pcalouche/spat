package com.pcalouche.spat.security.authentication;

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
                                  Collection<? extends GrantedAuthority> authorities) {
        super(authorities);
        this.subject = subject;
        this.token = null;
        setAuthenticated(true);
    }

    @Override
    public String getPrincipal() {
        return subject;
    }

    @Override
    public String getCredentials() {
        return token;
    }
}
