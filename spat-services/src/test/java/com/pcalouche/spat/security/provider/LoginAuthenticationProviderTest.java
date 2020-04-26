package com.pcalouche.spat.security.provider;

import com.pcalouche.spat.entity.User;
import com.pcalouche.spat.repository.UserRepository;
import com.pcalouche.spat.security.authentication.JwtAuthenticationToken;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

@ExtendWith(SpringExtension.class)
public class LoginAuthenticationProviderTest {
    private LoginAuthenticationProvider loginAuthenticationProvider;
    @MockBean
    private UserRepository userRepository;

    @BeforeEach
    public void before() {
        Mockito.reset(userRepository);

        User activeUser = User.builder()
                .username("activeUser")
                .password("$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi")
                .build();

        given(userRepository.findByUsername(activeUser.getUsername())).willReturn(Optional.of(activeUser));

        given(userRepository.findByUsername("bogusUser")).willReturn(Optional.empty());

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

        verify(userRepository, Mockito.times(1)).findByUsername("activeUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForNullUser() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("bogusUser", "password");

        assertThatThrownBy(() -> loginAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: bogusUser");

        verify(userRepository, Mockito.times(1)).findByUsername("bogusUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForBadPassword() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeUser", "badPassword");

        assertThatThrownBy(() -> loginAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: activeUser");

        verify(userRepository, Mockito.times(1)).findByUsername("activeUser");
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
