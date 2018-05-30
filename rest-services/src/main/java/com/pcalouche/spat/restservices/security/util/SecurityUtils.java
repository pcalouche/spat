package com.pcalouche.spat.restservices.security.util;

import com.pcalouche.spat.restservices.api.BaseEndpoints;
import com.pcalouche.spat.restservices.api.dto.AuthResponseDto;
import com.pcalouche.spat.restservices.api.entity.User;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import org.springframework.http.HttpHeaders;
import org.springframework.security.authentication.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

import javax.servlet.http.HttpServletRequest;
import java.util.Base64;
import java.util.Date;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class SecurityUtils {
    public static final PasswordEncoder PASSWORD_ENCODER = new BCryptPasswordEncoder();
    public static final String AUTHENTICATED_PATH = String.format("%s/**", BaseEndpoints.API_ROOT);
    public static final String TOKEN_ENDPOINT = String.format("%s/auth/token", BaseEndpoints.API_ROOT);
    public static final String REFRESH_TOKEN_ENDPOINT = String.format("%s/auth/refresh-token", BaseEndpoints.API_ROOT);
    public static final String AUTH_HEADER_BASIC_PREFIX = "Basic ";
    public static final String AUTH_HEADER_BEARER_PREFIX = "Bearer ";
    public static final String CLAIMS_AUTHORITIES_KEY = "authorities";
    private static final String SIGNING_KEY = "farside597";
    private static final long TOKEN_DURATION_IN_MINUTES = 10L;
    private static final long REFRESH_TOKEN_DURATION_IN_MINUTES = 15L;

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

    public static Claims getClaimsFromToken(String token) {
        return Jwts.parser()
                .setSigningKey(SIGNING_KEY)
                .parseClaimsJws(token)
                .getBody();
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

    public static AuthResponseDto createAuthResponse(Authentication authentication) {
        Date now = new Date();
        String tokenId = UUID.randomUUID().toString();
        Date tokenExpiration = new Date(now.getTime() + TimeUnit.MINUTES.toMillis(TOKEN_DURATION_IN_MINUTES));
        Date refreshTokenExpiration = new Date(now.getTime() + TimeUnit.MINUTES.toMillis(REFRESH_TOKEN_DURATION_IN_MINUTES));

        return new AuthResponseDto(
                createToken(authentication, tokenId, now, tokenExpiration),
                createToken(authentication, tokenId, now, refreshTokenExpiration)
        );
    }

    private static String createToken(Authentication authentication, String tokenId, Date now, Date expiration) {
        Claims claims = Jwts.claims();
        claims.setIssuer("com.pcalouche.spat");
        claims.setId(tokenId);
        claims.setSubject(authentication.getName());
        claims.setIssuedAt(now);
        claims.setExpiration(expiration);
        // Add additional information to the JWT claims
        claims.put(CLAIMS_AUTHORITIES_KEY, authentication.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .collect(Collectors.toSet()));
        return Jwts.builder()
                .setClaims(claims)
                .signWith(SignatureAlgorithm.HS256, SIGNING_KEY)
                .compact();
    }
}
