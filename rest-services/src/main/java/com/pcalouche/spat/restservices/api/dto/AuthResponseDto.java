package com.pcalouche.spat.restservices.api.dto;

public class AuthResponseDto {
    private final String token;
    private final String refreshToken;

    public AuthResponseDto(String token, String refreshToken) {
        this.token = token;
        this.refreshToken = refreshToken;
    }

    public String getToken() {
        return token;
    }

    public String getRefreshToken() {
        return refreshToken;
    }
}
