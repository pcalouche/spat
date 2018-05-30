package com.pcalouche.spat.restservices.security.provider;

import com.pcalouche.spat.restservices.api.dto.AuthResponseDto;
import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.shared.AbstractUnitTest;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
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
    private User activeUser;

    @Before
    public void before() {
        Mockito.reset(userRepository);
        Set<Role> roles = new HashSet<>();
        roles.add(new Role(1L, "ROLE_USER"));
        activeUser = new User(1L, "activeUser", roles);

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
    public void testAuthenticateHandlesRefreshToken() {
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
    public void testAuthenticateRefreshTokenHandlesDisabledUserAccount() {
        // Disable the active user.  They should not be given a refresh token
        activeUser.setEnabled(false);
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validJwtToken);
        authenticationToken.setDetails("refreshToken");

        // All other cases are tested in SecurityUtilsTest.  This just checks that the conditional is hit
        assertThatThrownBy(() -> jwtAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(DisabledException.class)
                .hasMessage("Disabled account for username: activeUser");

        // User service should be hit for non refresh token
        verify(userRepository, Mockito.times(1)).findByUsername("activeUser");

    }

    @Test
    public void testAuthenticateRefreshTokenHandlesDeletedUserAccount() {
        // Simulate that the user was deleted from the database since they last received a token
        given(userRepository.findByUsername(activeUser.getUsername())).willReturn(null);
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validJwtToken);
        authenticationToken.setDetails("refreshToken");

        // All other cases are tested in SecurityUtilsTest.  This just checks that the conditional is hit
        assertThatThrownBy(() -> jwtAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: activeUser");

        // User service should be hit for non refresh token
        verify(userRepository, Mockito.times(1)).findByUsername("activeUser");
    }

    @Test
    public void testSupportValidAuthenticationClass() {
        assertThat(jwtAuthenticationProvider.supports(JwtAuthenticationToken.class))
                .isTrue();
    }

    @Test
    public void testSupportInvalidAuthenticationClass() {
        assertThat(jwtAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class))
                .isFalse();
    }
}
