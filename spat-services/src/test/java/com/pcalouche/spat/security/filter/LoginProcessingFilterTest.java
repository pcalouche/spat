package com.pcalouche.spat.security.filter;

import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.security.provider.LoginAuthenticationProvider;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockServletContext;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.Base64;
import java.util.Collections;
import java.util.HashSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(SpringExtension.class)
public class LoginProcessingFilterTest {
    @MockBean
    private LoginAuthenticationProvider loginAuthenticationProvider;
    @MockBean
    private FilterChain filterChain;
    private LoginProcessingFilter loginProcessingFilter;

    @BeforeEach
    public void before() {
        given(loginAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class)).willCallRealMethod();
        UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken = new UsernamePasswordAuthenticationToken(
                "activeUser",
                "password"
        );
        given(loginAuthenticationProvider.authenticate(usernamePasswordAuthenticationToken)).willReturn(
                new UsernamePasswordAuthenticationToken("activeUser", null, new HashSet<>())
        );

        AuthenticationManager authenticationManager = new ProviderManager(Collections.singletonList(loginAuthenticationProvider));

        loginProcessingFilter = new LoginProcessingFilter(authenticationManager);
    }

    @Test
    public void testExpectedPathsAreAuthenticated() throws IOException, ServletException {
        MockHttpServletRequest request = MockMvcRequestBuilders.post(Endpoints.AUTH + Endpoints.TOKEN)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes()))
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        // Use an endpoint that is not white listed for testing
        assertThatCode(() -> loginProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once along with the ajaxLoginAuthenticationProvider
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(loginAuthenticationProvider, times(1)).authenticate(any());
    }

    @Test
    public void testWhiteListedPathsAreNotAuthenticated() throws IOException, ServletException {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(SecurityUtils.WHITELISTED_ENDPOINTS[1])
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        // Use an endpoint that is white listed for testing
        assertThatCode(() -> loginProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once, but the ajaxLoginAuthenticationProvider should have
        // been called 0 times since authentication should not be done on white listed endpoints
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(loginAuthenticationProvider, times(0)).authenticate(any());
    }

    @Test
    public void testAttemptAuthentication() {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(Endpoints.AUTH + Endpoints.TOKEN)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes()))
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        Authentication authentication = loginProcessingFilter.attemptAuthentication(request, response);

        assertThat(authentication.getName()).isEqualTo("activeUser");
        assertThat(authentication.getCredentials()).isNull();
        assertThat(authentication.getAuthorities()).isEmpty();
    }
}
