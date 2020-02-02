package com.pcalouche.spat.restservices.security.provider;

import com.pcalouche.spat.restservices.AbstractTest;
import com.pcalouche.spat.restservices.entity.User;
import com.pcalouche.spat.restservices.repository.UserRepository;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

public class LoginAuthenticationProviderTest extends AbstractTest {
    private LoginAuthenticationProvider loginAuthenticationProvider;
    @MockBean
    private UserRepository userRepository;

    @Before
    public void before() {
        Mockito.reset(userRepository);

        User activeUser = User.builder()
                .username("activeUser")
                .password("$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi")
                .build();

        given(userRepository.findById(activeUser.getUsername())).willReturn(Optional.of(activeUser));

        given(userRepository.findById("bogusUser")).willReturn(Optional.empty());

        loginAuthenticationProvider = new LoginAuthenticationProvider(userRepository);
    }

    @Test
    public void testAuthenticate() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeUser", "password");
        Authentication authentication = loginAuthenticationProvider.authenticate(authenticationToken);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEmpty();

        verify(userRepository, Mockito.times(1)).findById("activeUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForNullUser() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("bogusUser", "password");

        assertThatThrownBy(() -> loginAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: bogusUser");

        verify(userRepository, Mockito.times(1)).findById("bogusUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForBadPassword() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeUser", "badPassword");

        assertThatThrownBy(() -> loginAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: activeUser");

        verify(userRepository, Mockito.times(1)).findById("activeUser");
    }

    @Test
    public void testSupportValidAuthenticationClass() {
        assertThat(loginAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class))
                .isTrue();
    }

    @Test
    public void testSupportInvalidAuthenticationClass() {
        assertThat(loginAuthenticationProvider.supports(JwtAuthenticationToken.class))
                .isFalse();
    }
}
