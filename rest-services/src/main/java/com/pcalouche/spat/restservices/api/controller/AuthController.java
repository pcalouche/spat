package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.Endpoints;
import com.pcalouche.spat.restservices.api.dto.AuthResponseDto;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import io.jsonwebtoken.Claims;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpHeaders;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "Auth endpoints")
@RestController
@RequestMapping(value = Endpoints.AUTH)
public class AuthController {

    @Operation(description = "Get JWT from username and password")
    @PostMapping(value = Endpoints.TOKEN)
    public AuthResponseDto token(@AuthenticationPrincipal Authentication authentication) {
        return SecurityUtils.createAuthResponse(authentication);
    }

    @Operation(description = "Get JWT from refresh token")
    @PostMapping(value = Endpoints.REFRESH_TOKEN)
    public AuthResponseDto refreshToken(@AuthenticationPrincipal Authentication authentication,
                                        @RequestHeader(HttpHeaders.AUTHORIZATION) String authHeader) {
        // Grab latest user details and build a token from that.
        Claims claims = SecurityUtils.getClaimsFromToken(authHeader.replace("Bearer ", ""));
        //        if (Boolean.TRUE.equals()!Boolean.valueOf(claims.get(SecurityUtils.CLAIMS_REFRESH_TOKEN_KEY).toString())) {
        if (!Boolean.parseBoolean(claims.get(SecurityUtils.CLAIMS_REFRESH_TOKEN_KEY).toString())) {
            throw new BadCredentialsException("Non refresh token used");
        }
        return SecurityUtils.createAuthResponse(authentication);
    }
}
