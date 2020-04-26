package com.pcalouche.spat.security.util;

import com.pcalouche.spat.config.SpatProperties;
import com.pcalouche.spat.entity.User;
import io.jsonwebtoken.Claims;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockServletContext;
import org.springframework.security.authentication.*;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import java.util.Base64;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@ExtendWith(SpringExtension.class)
@EnableConfigurationProperties(value = SpatProperties.class)
@TestPropertySource("classpath:application-test.properties")
public class SecurityUtilsTest {
    private SecurityUtils securityUtils;
    @Autowired
    private SpatProperties spatProperties;

    @BeforeEach
    public void before() {
        securityUtils = new SecurityUtils(spatProperties);
    }

    @Test
    public void testGetDecodedBasicAuthFromRequest() {
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + "YWN0aXZlVXNlcjpwYXNzd29yZA==")
                .buildRequest(new MockServletContext());
        String[] decodedParts = SecurityUtils.getDecodedBasicAuthFromRequest(request);
        assertThat(decodedParts).hasSize(2);
        assertThat(decodedParts[0]).isEqualTo("activeUser");
        assertThat(decodedParts[1]).isEqualTo("password");
    }

    @Test
    public void testGetDecodedBasicAuthFromRequestBadDecodeThrowsException() {
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .buildRequest(new MockServletContext());
        assertThatThrownBy(() -> SecurityUtils.getDecodedBasicAuthFromRequest(request))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Basic Authentication header is invalid");
    }

    @Test
    public void testGetDecodedBasicAuthFromRequestBadHeaderPrefixThrowsException() {
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("invalidHeader".getBytes()))
                .buildRequest(new MockServletContext());

        assertThatThrownBy(() -> SecurityUtils.getDecodedBasicAuthFromRequest(request))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Basic Authentication header is invalid");
    }

    @Test
    public void testGetTokenFromRequest() {
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "YWN0aXZlVXNlcjpwYXNzd29yZA==")
                .buildRequest(new MockServletContext());

        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "YWN0aXZlVXNlcjpwYXNzd29yZA==");
        assertThat(SecurityUtils.getTokenFromRequest(request)).isNotEmpty();
    }

    @Test
    public void testValidateUserAccountStatusAccountExpiredException() {
        User user = User.builder()
                .username("expiredUser")
                .accountNonExpired(false)
                .build();
        user.setAccountNonExpired(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(AccountExpiredException.class)
                .hasMessage(String.format("Expired account for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusAccountLockedException() {
        User user = User.builder()
                .username("lockedUser")
                .accountNonLocked(false)
                .build();
        user.setAccountNonLocked(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(LockedException.class)
                .hasMessage(String.format("Locked account for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusCredentialsException() {
        User user = User.builder()
                .username("credentialsExpiredUser")
                .credentialsNonExpired(false)
                .build();
        user.setCredentialsNonExpired(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(CredentialsExpiredException.class)
                .hasMessage(String.format("Credentials expired for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusDisabledException() {
        User user = User.builder()
                .username("disabledUser")
                .enabled(false)
                .build();
        user.setEnabled(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(DisabledException.class)
                .hasMessage(String.format("Disabled account for username: %s", user.getUsername()));
    }

    @Test
    public void testCreateToken() {
        Set<SimpleGrantedAuthority> authorities = Stream.of(new SimpleGrantedAuthority("Admin")).collect(Collectors.toSet());
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeAdmin", "password", authorities);

        String token = securityUtils.createToken(authenticationToken);
        Claims claims = securityUtils.getClaimsFromToken(token);

        assertThat(claims.getIssuer()).isEqualTo("com.pcalouche.spat");
        assertThat(claims.getSubject()).isEqualTo("activeAdmin");
        assertThat(claims.getId()).isNotBlank();
        assertThat(claims.getIssuedAt()).isNotNull();
        assertThat(claims.getExpiration()).isNotNull();
        assertThat(claims.getIssuedAt().getTime())
                .isEqualTo(claims.getExpiration().getTime() - spatProperties.getJwtTokenDuration().toMillis());
        @SuppressWarnings("unchecked")
        List<String> tokenAuthorities = (List<String>) claims.get("authorities", List.class);
        assertThat(tokenAuthorities).containsExactly("Admin");
        assertThat(Boolean.parseBoolean(claims.get("refreshToken").toString())).isFalse();
    }

    @Test
    public void testCreateRefreshTokenCookie() {
        ResponseCookie responseCookie = securityUtils.createRefreshTokenCookie("activeUser");
        assertThat(responseCookie.getValue()).isNotBlank();
        assertThat(responseCookie.getDomain()).isEqualTo(spatProperties.getHostname());
        assertThat(responseCookie.getPath()).isEqualTo("/");
        assertThat(responseCookie.isHttpOnly()).isEqualTo(true);
        assertThat(responseCookie.isSecure()).isEqualTo(spatProperties.isHttpsEnvironment());

        Claims claims = securityUtils.getClaimsFromToken(responseCookie.getValue());

        assertThat(claims.getIssuer()).isEqualTo("com.pcalouche.spat");
        assertThat(claims.getSubject()).isEqualTo("activeUser");
        assertThat(claims.getId()).isNotBlank();
        assertThat(claims.getIssuedAt()).isNotNull();
        assertThat(claims.getExpiration()).isNotNull();
        assertThat(claims.getIssuedAt().getTime())
                .isEqualTo(claims.getExpiration().getTime() - spatProperties.getRefreshTokenDuration().toMillis());
        assertThat(Boolean.parseBoolean(claims.get("refreshToken").toString())).isTrue();
    }

    @Test
    public void testDeleteRefreshTokenCookie() {
        ResponseCookie responseCookie = securityUtils.deleteRefreshTokenCookie();
        assertThat(responseCookie.getValue()).isBlank();
        assertThat(responseCookie.getDomain()).isEqualTo(spatProperties.getHostname());
        assertThat(responseCookie.getPath()).isEqualTo("/");
        assertThat(responseCookie.isHttpOnly()).isEqualTo(true);
        assertThat(responseCookie.isSecure()).isEqualTo(spatProperties.isHttpsEnvironment());
        assertThat(responseCookie.getMaxAge().toMillis()).isEqualTo(0);
    }

    @Test
    public void testGetClaimsFromToken() {
        Set<SimpleGrantedAuthority> authorities = Stream.of(new SimpleGrantedAuthority("Admin")).collect(Collectors.toSet());
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeAdmin", "password", authorities);
        Claims claims = securityUtils.getClaimsFromToken(securityUtils.createToken(authenticationToken));
        assertThat(claims.getSubject()).isEqualTo("activeAdmin");
        assertThat(claims.getId()).isNotEmpty();
        assertThat(claims.getIssuedAt()).isNotNull();
        assertThat(claims.getExpiration()).isNotNull();
        @SuppressWarnings("unchecked")
        List<String> tokenAuthorities = (List<String>) claims.get("authorities", List.class);
        assertThat(tokenAuthorities).hasSize(1);
        assertThat(tokenAuthorities.get(0)).isEqualTo("Admin");
    }
}
