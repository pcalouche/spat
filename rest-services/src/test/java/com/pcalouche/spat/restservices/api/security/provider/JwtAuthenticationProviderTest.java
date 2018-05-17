package com.pcalouche.spat.restservices.api.security.provider;

import com.pcalouche.spat.restservices.api.dto.AuthResponseDto;
import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

public class JwtAuthenticationProviderTest extends AbstractUnitTest {
    private JwtAuthenticationProvider jwtAuthenticationProvider;
    @MockBean
    private UserRepository userRepository;
    private String validJwtToken;

    @Before
    public void before() {
        Mockito.reset(userRepository);
        Set<Role> roles = new HashSet<>();
        roles.add(new Role(1L, "ROLE_USER"));
        User activeUser = new User(1L, "activeUser", roles);

        activeUser.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));

        given(userRepository.findByUsername(activeUser.getUsername())).willReturn(activeUser);

        given(userRepository.findByUsername("bogusUser")).willReturn(null);

        jwtAuthenticationProvider = new JwtAuthenticationProvider(userRepository);

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
        verify(userRepository, Mockito.times(0)).findByUsername("activeUser");
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
        verify(userRepository, Mockito.times(1)).findByUsername("activeUser");
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
