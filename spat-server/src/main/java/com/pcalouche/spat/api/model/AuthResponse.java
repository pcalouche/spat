package com.pcalouche.spat.api.model;

import com.pcalouche.spat.security.util.SecurityUtils;

public class AuthResponse {
    private final String token;
    private final String refreshToken;

    public AuthResponse(String token, String refreshToken) {
        this.token = token;
        this.refreshToken = refreshToken;
    }

    public String getPrefix() {
        return SecurityUtils.AUTH_HEADER_BEARER_PREFIX;
    }

    public String getToken() {
        return token;
    }

    public String getRefreshToken() {
        return refreshToken;
    }
}
