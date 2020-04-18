package com.pcalouche.spat.security.util;

import com.pcalouche.spat.api.dto.AuthResponseDto;
import com.pcalouche.spat.config.SpatProperties;
import com.pcalouche.spat.entity.User;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import org.springframework.http.HttpHeaders;
import org.springframework.security.authentication.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import javax.servlet.http.HttpServletRequest;
import java.util.Base64;
import java.util.Date;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class SecurityUtils {
    public static final PasswordEncoder PASSWORD_ENCODER = new BCryptPasswordEncoder();
    public static final String[] WHITELISTED_ENDPOINTS = {
            // swagger endpoints
            "/csrf",
            "/error",
            "/v3/api-docs/**",
            "/swagger-ui.html",
            "/swagger-resources",
            "/swagger-resources/**",
            "/configuration/ui",
            "/configuration/security",
            "/swagger-ui/**",
            "/webjars/**",
            // status endpoint
            "/status"
    };
    public static final String AUTHENTICATED_PATH = "/**";
    public static final String AUTH_HEADER_BASIC_PREFIX = "Basic ";
    public static final String AUTH_HEADER_BEARER_PREFIX = "Bearer ";
    public static final String CLAIMS_AUTHORITIES_KEY = "authorities";
    public static final String CLAIMS_REFRESH_TOKEN_KEY = "refreshToken";
    private static final String SIGNING_KEY = "Vlg1A40XMNUCLuMZ4h5qh5K4li3P3lqgqSLhNVNOBHqM9ZSnirtV+Hlcl4VOjfLI/shtzqmNNAnB8v0tvECJMQ==";
    private final SecretKey secretKey;
    private final SpatProperties spatProperties;

    public SecurityUtils(SpatProperties spatProperties) {
        if (spatProperties.getJwtTokenDuration().compareTo(spatProperties.getRefreshTokenDuration()) > 0) {
            throw new IllegalArgumentException("jwt token duration cannot be greater than refresh token duration");
        }
        this.spatProperties = spatProperties;
        this.secretKey = Keys.hmacShaKeyFor(SIGNING_KEY.getBytes());
    }

    public static String[] getDecodedBasicAuthFromRequest(HttpServletRequest request) throws AuthenticationException {
        String headerValue = request.getHeader(HttpHeaders.AUTHORIZATION);
        if (headerValue != null && headerValue.startsWith(AUTH_HEADER_BASIC_PREFIX)) {
            String encodedBase64String = headerValue.replace(AUTH_HEADER_BASIC_PREFIX, "");
            String[] decodedBasicAuthorizationParts = new String(Base64.getDecoder().decode(encodedBase64String)).split(":");
            if (decodedBasicAuthorizationParts.length != 2) {
                throw new BadCredentialsException("Basic Authentication header is invalid");
            } else {
                return decodedBasicAuthorizationParts;
            }
        } else {
            throw new BadCredentialsException("Basic Authentication header is invalid");
        }
    }

    public static String getTokenFromRequest(HttpServletRequest request) {
        String token = request.getHeader(HttpHeaders.AUTHORIZATION);
        if (token != null && token.startsWith(AUTH_HEADER_BEARER_PREFIX)) {
            token = token.replace(AUTH_HEADER_BEARER_PREFIX, "");
        }
        return token;
    }

    public static void validateUserAccountStatus(User user) {
        if (!user.isAccountNonExpired()) {
            throw new AccountExpiredException(String.format("Expired account for username: %s", user.getUsername()));
        } else if (!user.isAccountNonLocked()) {
            throw new LockedException(String.format("Locked account for username: %s", user.getUsername()));
        } else if (!user.isCredentialsNonExpired()) {
            throw new CredentialsExpiredException(String.format("Credentials expired for username: %s", user.getUsername()));
        } else if (!user.isEnabled()) {
            throw new DisabledException(String.format("Disabled account for username: %s", user.getUsername()));
        }
    }

    public Claims getClaimsFromToken(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(secretKey)
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    public AuthResponseDto createAuthResponse(String subject, Set<String> authorities) {
        Date now = new Date();
        String tokenId = UUID.randomUUID().toString();
        Date tokenExpiration = new Date(now.getTime() + spatProperties.getJwtTokenDuration().toMillis());
        Date refreshTokenExpiration = new Date(now.getTime() + spatProperties.getRefreshTokenDuration().toMillis());

        return new AuthResponseDto(
                createToken(subject, authorities, tokenId, now, tokenExpiration, false),
                createToken(subject, authorities, tokenId, now, refreshTokenExpiration, true)
        );
    }

    public AuthResponseDto createAuthResponse(Authentication authentication) {
        Set<String> authorities = authentication.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .collect(Collectors.toSet());
        return createAuthResponse(authentication.getName(), authorities);
    }

    private String createToken(String subject,
                               Set<String> authorities,
                               String tokenId,
                               Date now,
                               Date expiration,
                               boolean refreshToken) {
        Claims claims = Jwts.claims();
        claims.setIssuer("com.pcalouche.spat");
        claims.setId(tokenId);
        claims.setSubject(subject);
        claims.setIssuedAt(now);
        claims.setExpiration(expiration);
        claims.put(CLAIMS_REFRESH_TOKEN_KEY, refreshToken);
        // Add additional information to the JWT claims
        claims.put(CLAIMS_AUTHORITIES_KEY, authorities);
        return Jwts.builder()
                .setClaims(claims)
                .signWith(secretKey)
                .compact();
    }
}