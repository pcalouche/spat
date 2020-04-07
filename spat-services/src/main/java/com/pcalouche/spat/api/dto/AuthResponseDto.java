package com.pcalouche.spat.api.dto;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public class AuthResponseDto {
    private final String token;
    private final String refreshToken;
}
