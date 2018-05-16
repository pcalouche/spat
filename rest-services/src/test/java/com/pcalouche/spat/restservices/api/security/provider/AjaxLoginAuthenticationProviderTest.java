package com.pcalouche.spat.restservices.api.security.provider;

import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.provider.AjaxLoginAuthenticationProvider;
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
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

public class AjaxLoginAuthenticationProviderTest extends AbstractUnitTest {
    private AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider;
    @MockBean
    private UserRepository userRepository;

    @Before
    public void before() {
        Mockito.reset(userRepository);

        Set<Role> roles = new HashSet<>();
        roles.add(new Role("ROLE_USER"));
        User activeUser = new User(1L, "activeUser", roles);
        activeUser.setPassword("$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi");

        given(userRepository.findByUsername(activeUser.getUsername())).willReturn(activeUser);

        given(userRepository.findByUsername("bogusUser")).willReturn(null);

        ajaxLoginAuthenticationProvider = new AjaxLoginAuthenticationProvider(userRepository);
    }

    @Test
    public void testAuthenticate() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeUser", "password");
        Authentication authentication = ajaxLoginAuthenticationProvider.authenticate(authenticationToken);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));

        verify(userRepository, Mockito.times(1)).findByUsername("activeUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForNullUser() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("bogusUser", "password");

        assertThatThrownBy(() -> ajaxLoginAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: bogusUser");

        verify(userRepository, Mockito.times(1)).findByUsername("bogusUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForBadPassword() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeUser", "badPassword");

        assertThatThrownBy(() -> ajaxLoginAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: activeUser");

        verify(userRepository, Mockito.times(1)).findByUsername("activeUser");
    }

    @Test
    public void testSupportValidAuthenticationClass() {
        assertThat(ajaxLoginAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class))
                .isTrue();
    }

    @Test
    public void testSupportInvalidAuthenticationClass() {
        assertThat(ajaxLoginAuthenticationProvider.supports(JwtAuthenticationToken.class))
                .isFalse();
    }
}
