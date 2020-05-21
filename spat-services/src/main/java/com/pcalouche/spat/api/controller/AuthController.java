package com.pcalouche.spat.api.controller;

import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.security.util.SecurityUtils;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "Auth endpoints")
@RestController
@RequestMapping(value = Endpoints.AUTH)
public class AuthController {
    private final SecurityUtils securityUtils;

    public AuthController(SecurityUtils securityUtils) {
        this.securityUtils = securityUtils;
    }

    @Operation(description = "Get JWT from username and password")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "token and refresh token created")})
    @PostMapping(value = Endpoints.TOKEN)
    public ResponseEntity<String> token(@AuthenticationPrincipal UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken) {
        System.out.println("goaafdsf");
        return ResponseEntity.ok()
                .header(HttpHeaders.SET_COOKIE, securityUtils.createRefreshTokenCookie(usernamePasswordAuthenticationToken.getName()).toString())
                .body(securityUtils.createToken(usernamePasswordAuthenticationToken));
    }

    @Operation(description = "Deletes a JWT by removing the refresh_token cookie. This will force a client to log back in.")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "refresh cookie was deleted")})
    @DeleteMapping(value = Endpoints.TOKEN)
    public ResponseEntity<Void> deleteToken() {
        return ResponseEntity.ok()
                .header(HttpHeaders.SET_COOKIE, securityUtils.deleteRefreshTokenCookie().toString())
                .build();
    }

    @Operation(description = "Get JWT from refresh token")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "token and refresh token created")})
    @PostMapping(value = Endpoints.REFRESH_TOKEN)
    public ResponseEntity<String> refreshToken(@AuthenticationPrincipal JwtAuthenticationToken jwtAuthenticationToken) {
        return ResponseEntity.ok()
                .header(HttpHeaders.SET_COOKIE, securityUtils.createRefreshTokenCookie(jwtAuthenticationToken.getName()).toString())
                .body(securityUtils.createToken(jwtAuthenticationToken));
    }
}
