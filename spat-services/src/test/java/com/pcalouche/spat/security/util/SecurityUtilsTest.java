package com.pcalouche.spat.security.util;

import com.pcalouche.spat.api.dto.AuthResponseDto;
import com.pcalouche.spat.entity.User;
import io.jsonwebtoken.Claims;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockServletContext;
import org.springframework.security.authentication.*;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import java.util.Base64;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class SecurityUtilsTest {

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
    public void testGetClaimsFromToken() {
        Set<SimpleGrantedAuthority> authorities = Stream.of(new SimpleGrantedAuthority("Admin")).collect(Collectors.toSet());
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeAdmin", "pretendToken", authorities);
        AuthResponseDto authResponseDto = SecurityUtils.createAuthResponse(authenticationToken);
        Claims claims = SecurityUtils.getClaimsFromToken(authResponseDto.getToken());
        assertThat(claims.getSubject()).isEqualTo("activeAdmin");
        assertThat(claims.getId()).isNotEmpty();
        assertThat(claims.getIssuedAt()).isNotNull();
        assertThat(claims.getExpiration()).isNotNull();
        List<String> tokenAuthorities = (List<String>) claims.get("authorities", List.class);
        assertThat(tokenAuthorities).hasSize(1);
        assertThat(tokenAuthorities.get(0)).isEqualTo("Admin");
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
    public void testCreateAuthResponse() {
        Set<SimpleGrantedAuthority> authorities = Stream.of(new SimpleGrantedAuthority("Admin")).collect(Collectors.toSet());
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeAdmin", "pretendToken", authorities);
        AuthResponseDto authResponseDto = SecurityUtils.createAuthResponse(authenticationToken);
        assertThat(authResponseDto.getToken()).isNotBlank();
        assertThat(authResponseDto.getRefreshToken()).isNotBlank();
    }
}
