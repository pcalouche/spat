package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.dto.AuthResponseDto;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import io.jsonwebtoken.Claims;
import io.swagger.annotations.ApiOperation;
import org.springframework.http.HttpHeaders;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = ApiEndpoints.AUTH)
public class AuthController extends AbstractSpatController {

    @ApiOperation(value = "Get JWT from username and password")
    @PostMapping(value = ApiEndpoints.TOKEN)
    public AuthResponseDto token(@AuthenticationPrincipal Authentication authentication) {
        return SecurityUtils.createAuthResponse(authentication);
    }

    @ApiOperation(value = "Get JWT from refresh token")
    @PostMapping(value = ApiEndpoints.REFRESH_TOKEN)
    public AuthResponseDto refreshToken(@AuthenticationPrincipal Authentication authentication,
                                        @RequestHeader(HttpHeaders.AUTHORIZATION) String authHeader) {
        // Grab latest user details and build a token from that.
        Claims claims = SecurityUtils.getClaimsFromToken(authHeader.replace("Bearer ", ""));
        if (!Boolean.valueOf(claims.get(SecurityUtils.CLAIMS_REFRESH_TOKEN_KEY).toString())) {
            throw new BadCredentialsException("Non refresh token used");
        }
        return SecurityUtils.createAuthResponse(authentication);
    }
}
