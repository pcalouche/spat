package com.pcalouche.spat.restservices.api.security.provider;

import com.pcalouche.spat.restservices.api.dto.AuthResponseDto;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.shared.AbstractUnitTest;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

public class JwtAuthenticationProviderTest extends AbstractUnitTest {
    private JwtAuthenticationProvider jwtAuthenticationProvider;
    @MockBean
    private UserService userService;
    private String validJwtToken;

    @Before
    public void before() {
        Mockito.reset(userService);
        User activeUser = new User(1L, "activeUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        activeUser.setPassword("$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi");
        given(userService.getByUsername(activeUser.getUsername())).willReturn(activeUser);

        given(userService.getByUsername("bogusUser")).willReturn(null);

        jwtAuthenticationProvider = new JwtAuthenticationProvider(userService);

        List<SimpleGrantedAuthority> authorities = Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER"));
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken("activeUser", "pretendToken", authorities);
        AuthResponseDto authResponseDto = SecurityUtils.createAuthResponse(authenticationToken);
        validJwtToken = authResponseDto.getToken();
    }

    @Test
    public void testAuthenticate() {
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validJwtToken);

        Authentication authentication = jwtAuthenticationProvider.authenticate(authenticationToken);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));

        // User service should not be hit for non refresh token
        verify(userService, Mockito.times(0)).getByUsername("activeUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForNullToken() {
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(null);

        assertThatThrownBy(() -> jwtAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("JSON web token was empty.");
    }

    @Test
    public void testAuthenticateThrowsHandlesRefreshToken() {
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validJwtToken);
        authenticationToken.setDetails("refreshToken");

        Authentication authentication = jwtAuthenticationProvider.authenticate(authenticationToken);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));

        // User service should be hit for non refresh token
        verify(userService, Mockito.times(1)).getByUsername("activeUser");
    }

    @Test
    public void testSupportValidationAuthenticationClass() {
        assertThat(jwtAuthenticationProvider.supports(JwtAuthenticationToken.class))
                .isTrue();
    }

    @Test
    public void testSupportInValidationAuthenticationClass() {
        assertThat(jwtAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class))
                .isFalse();
    }
}
