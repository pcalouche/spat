package com.pcalouche.spat.security.provider;

import com.pcalouche.spat.config.SpatProperties;
import com.pcalouche.spat.entity.User;
import com.pcalouche.spat.repository.UserRepository;
import com.pcalouche.spat.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.HashSet;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

@ExtendWith(SpringExtension.class)
@Import({
        SecurityUtils.class
})
@EnableConfigurationProperties(value = SpatProperties.class)
@TestPropertySource("classpath:application-test.properties")
public class JwtAuthenticationProviderTest {
    @Autowired
    private SecurityUtils securityUtils;
    private JwtAuthenticationProvider jwtAuthenticationProvider;
    @MockBean
    private UserRepository userRepository;
    private String validJwtToken;
    private String validRefreshToken;
    private User activeUser;

    @BeforeEach
    public void before() {
        Mockito.reset(userRepository);

        activeUser = User.builder()
                .username("activeUser")
                .build();

        activeUser.setPassword(SecurityUtils.PASSWORD_ENCODER.encode("password"));

        given(userRepository.findByUsername(activeUser.getUsername())).willReturn(Optional.ofNullable(activeUser));

        given(userRepository.findByUsername("bogusUser")).willReturn(Optional.empty());

        jwtAuthenticationProvider = new JwtAuthenticationProvider(securityUtils, userRepository);

        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken("activeUser", new HashSet<>());
        validJwtToken = securityUtils.createToken(authenticationToken);
        validRefreshToken = securityUtils.createRefreshTokenCookie(authenticationToken.getName()).getValue();
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
                .isEmpty();

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
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validRefreshToken);

        Authentication authentication = jwtAuthenticationProvider.authenticate(authenticationToken);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEmpty();

        // User service should be hit for non refresh token
        verify(userRepository, Mockito.times(1)).findByUsername("activeUser");
    }

    @Test
    public void testAuthenticateRefreshTokenHandlesDisabledUserAccount() {
        // Disable the active user.  They should not be given a refresh token
        activeUser.setEnabled(false);
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validRefreshToken);

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
        given(userRepository.findByUsername(activeUser.getUsername())).willReturn(Optional.empty());
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validRefreshToken);

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
