package com.pcalouche.spat.restservices.api.security.util;

import com.pcalouche.spat.restservices.api.dto.AuthResponseDto;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.shared.AbstractUnitTest;
import io.jsonwebtoken.Claims;
import org.junit.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.security.authentication.*;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class SecurityUtilsTest extends AbstractUnitTest {

    @Test
    public void testGetDecodedBasicAuthFromRequest() {
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + "YWN0aXZlVXNlcjpwYXNzd29yZA==");
        String[] decodedParts = SecurityUtils.getDecodedBasicAuthFromRequest(request);
        assertThat(decodedParts).hasSize(2);
        assertThat(decodedParts[0]).isEqualTo("activeUser");
        assertThat(decodedParts[1]).isEqualTo("password");
    }

    @Test
    public void testGetDecodedBasicAuthFromRequestBadDecodeThrowsException() {
        MockHttpServletRequest request = new MockHttpServletRequest();
        assertThatThrownBy(() -> SecurityUtils.getDecodedBasicAuthFromRequest(request))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Basic Authentication header is invalid");
    }

    @Test
    public void testGetDecodedBasicAuthFromRequestBadHeaderPrefixThrowsException() {
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("invalidHeader".getBytes()));
        assertThatThrownBy(() -> SecurityUtils.getDecodedBasicAuthFromRequest(request))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Basic Authentication header is invalid");
    }

    @Test
    public void testGetTokenFromRequest() {
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "YWN0aXZlVXNlcjpwYXNzd29yZA==");
        assertThat(SecurityUtils.getTokenFromRequest(request))
                .isNotEmpty();
    }

    @Test
    public void testGetClaimsFromToken() {
        List<SimpleGrantedAuthority> authorities = Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeAdmin", "pretendToken", authorities);
        AuthResponseDto authResponseDto = SecurityUtils.createAuthResponse(authenticationToken);
        Claims claims = SecurityUtils.getClaimsFromToken(authResponseDto.getToken());
        assertThat(claims.getSubject()).isEqualTo("activeAdmin");
        assertThat(claims.getId()).isNotEmpty();
        assertThat(claims.getIssuedAt()).isNotNull();
        assertThat(claims.getExpiration()).isNotNull();
        List<String> tokenAuthorities = (List<String>) claims.get("authorities", List.class);
        assertThat(tokenAuthorities)
                .hasSize(2);
        assertThat(tokenAuthorities.get(0)).isEqualTo("ROLE_USER");
        assertThat(tokenAuthorities.get(1)).isEqualTo("ROLE_ADMIN");
    }

    @Test
    public void testValidateUserAccountStatusAccountExpiredException() {
        User user = new User(1L, "expiredUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        user.setAccountNonExpired(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(AccountExpiredException.class)
                .hasMessage(String.format("Expired account for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusAccountLockedException() {
        User user = new User(1L, "lockedUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        user.setAccountNonLocked(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(LockedException.class)
                .hasMessage(String.format("Locked account for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusCredentialsException() {
        User user = new User(1L, "credentialsExpiredUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        user.setCredentialsNonExpired(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(CredentialsExpiredException.class)
                .hasMessage(String.format("Credentials expired for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusDisabledException() {
        User user = new User(1L, "disabledUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        user.setEnabled(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(DisabledException.class)
                .hasMessage(String.format("Disabled account for username: %s", user.getUsername()));

    }

    @Test
    public void testCreateAuthResponse() {
        List<SimpleGrantedAuthority> authorities = Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeAdmin", "pretendToken", authorities);
        AuthResponseDto authResponseDto = SecurityUtils.createAuthResponse(authenticationToken);
        assertThat(authResponseDto.getToken()).isNotBlank();
        assertThat(authResponseDto.getRefreshToken()).isNotBlank();
    }
}
