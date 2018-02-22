package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.security.util.SecurityUtils;

public class AuthResponseDto {
    private final String token;
    private final String refreshToken;

    public AuthResponseDto(String token, String refreshToken) {
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
