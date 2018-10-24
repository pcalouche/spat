package com.pcalouche.spat.restservices.security.util;

import com.pcalouche.spat.restservices.api.dto.AuthResponseDto;
import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.shared.AbstractUnitTest;
import io.jsonwebtoken.Claims;
import org.junit.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockServletContext;
import org.springframework.security.authentication.*;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class SecurityUtilsTest extends AbstractUnitTest {

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
        List<SimpleGrantedAuthority> authorities = Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeAdmin", "pretendToken", authorities);
        AuthResponseDto authResponseDto = SecurityUtils.createAuthResponse(authenticationToken);
        Claims claims = SecurityUtils.getClaimsFromToken(authResponseDto.getToken());
        assertThat(claims.getSubject()).isEqualTo("activeAdmin");
        assertThat(claims.getId()).isNotEmpty();
        assertThat(claims.getIssuedAt()).isNotNull();
        assertThat(claims.getExpiration()).isNotNull();
        List<String> tokenAuthorities = (List<String>) claims.get("authorities", List.class);
        assertThat(tokenAuthorities).hasSize(2);
        assertThat(tokenAuthorities.get(0)).isEqualTo("ROLE_USER");
        assertThat(tokenAuthorities.get(1)).isEqualTo("ROLE_ADMIN");
    }

    @Test
    public void testValidateUserAccountStatusAccountExpiredException() {
        Set<Role> roles = new HashSet<>();
        roles.add(Role.builder()
                .id(1)
                .name("ROLE_USER")
                .build());
        User user = User.builder()
                .username("expiredUser")
                .accountNonExpired(false)
                .roles(roles)
                .build();
        user.setAccountNonExpired(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(AccountExpiredException.class)
                .hasMessage(String.format("Expired account for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusAccountLockedException() {
        Set<Role> roles = new HashSet<>();
        roles.add(Role.builder()
                .id(1)
                .name("ROLE_USER")
                .build());
        User user = User.builder()
                .username("lockedUser")
                .accountNonLocked(false)
                .roles(roles)
                .build();
        user.setAccountNonLocked(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(LockedException.class)
                .hasMessage(String.format("Locked account for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusCredentialsException() {
        Set<Role> roles = new HashSet<>();
        roles.add(Role.builder()
                .id(1)
                .name("ROLE_USER")
                .build());
        User user = User.builder()
                .username("credentialsExpiredUser")
                .credentialsNonExpired(false)
                .roles(roles)
                .build();
        user.setCredentialsNonExpired(false);

        assertThatThrownBy(() -> SecurityUtils.validateUserAccountStatus(user))
                .isInstanceOf(CredentialsExpiredException.class)
                .hasMessage(String.format("Credentials expired for username: %s", user.getUsername()));
    }

    @Test
    public void testValidateUserAccountStatusDisabledException() {
        Set<Role> roles = new HashSet<>();
        roles.add(Role.builder()
                .id(1)
                .name("ROLE_USER")
                .build());
        User user = User.builder()
                .username("disabledUser")
                .enabled(false)
                .roles(roles)
                .build();
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
