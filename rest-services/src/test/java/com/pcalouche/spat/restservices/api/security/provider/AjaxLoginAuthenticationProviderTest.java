package com.pcalouche.spat.restservices.api.security.provider;

import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.service.UserService;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

public class AjaxLoginAuthenticationProviderTest extends AbstractUnitTest {
    private AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider;
    @MockBean
    private UserService userService;

    @Before
    public void before() {
        Mockito.reset(userService);
        User activeUser = new User(1L, "activeUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        activeUser.setPassword("$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi");
        given(userService.getByUsername(activeUser.getUsername())).willReturn(activeUser);

        given(userService.getByUsername("bogusUser")).willReturn(null);

        ajaxLoginAuthenticationProvider = new AjaxLoginAuthenticationProvider(userService);
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

        verify(userService, Mockito.times(1)).getByUsername("activeUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForNullUser() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("fakeUser", "password");

        assertThatThrownBy(() -> ajaxLoginAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: fakeUser");

        verify(userService, Mockito.times(1)).getByUsername("fakeUser");
    }

    @Test
    public void testAuthenticateThrowsBadCredentialsForBadPassword() {
        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeUser", "badPassword");

        assertThatThrownBy(() -> ajaxLoginAuthenticationProvider.authenticate(authenticationToken))
                .isInstanceOf(BadCredentialsException.class)
                .hasMessage("Bad credentials for username: activeUser");

        verify(userService, Mockito.times(1)).getByUsername("activeUser");
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
