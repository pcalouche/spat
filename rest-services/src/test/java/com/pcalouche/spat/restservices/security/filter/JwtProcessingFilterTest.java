package com.pcalouche.spat.restservices.security.filter;

import com.pcalouche.spat.restservices.AbstractTest;
import com.pcalouche.spat.restservices.api.Endpoints;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockServletContext;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.core.Authentication;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class JwtProcessingFilterTest extends AbstractTest {
    @MockBean
    private JwtAuthenticationProvider jwtAuthenticationProvider;
    @MockBean
    private FilterChain filterChain;
    private JwtProcessingFilter jwtProcessingFilter;

    @BeforeEach
    public void before() {
        given(jwtAuthenticationProvider.supports(JwtAuthenticationToken.class)).willCallRealMethod();

        JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("goodToken");
        given(jwtAuthenticationProvider.authenticate(jwtAuthenticationToken)).willReturn(
                new JwtAuthenticationToken("activeUser", null, new HashSet<>())
        );

        JwtAuthenticationToken jwtRefreshAuthenticationToken = new JwtAuthenticationToken("goodRefreshToken");
        jwtRefreshAuthenticationToken.setDetails("refreshToken");
        given(jwtAuthenticationProvider.authenticate(jwtRefreshAuthenticationToken)).willReturn(
                new JwtAuthenticationToken("activeUser", null, new HashSet<>())
        );

        AuthenticationManager authenticationManager = new ProviderManager(Collections.singletonList(jwtAuthenticationProvider));

        jwtProcessingFilter = new JwtProcessingFilter(authenticationManager);
    }

    @Test
    public void testExpectedPathsAreAuthenticated() throws IOException, ServletException {
        MockHttpServletRequest request = MockMvcRequestBuilders.post(Endpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "goodToken")
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        // Use an endpoint that is not white listed for testing
        assertThatCode(() -> jwtProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once along with the jwtAuthenticationProvider
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(jwtAuthenticationProvider, times(1)).authenticate(any());
    }

    @Test
    public void testTokenPathIsNotAuthenticated() throws IOException, ServletException {
        // Test that the token path is not authenticated because that is handled by the AjaxLoginProcessingFilter
        MockHttpServletRequest request = MockMvcRequestBuilders.post(Endpoints.AUTH + Endpoints.TOKEN)
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        assertThatCode(() -> jwtProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once, but the jwtAuthenticationProvider should have
        // been called 0 times since authentication should not be done on white listed endpoints
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(jwtAuthenticationProvider, times(0)).authenticate(any());
    }

    @Test
    public void testWhiteListedPathsAreNotAuthenticated() throws IOException, ServletException {
        // Test a white listed path
        MockHttpServletRequest request = MockMvcRequestBuilders.get(SecurityUtils.WHITELISTED_ENDPOINTS[2])
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        assertThatCode(() -> jwtProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once, but the jwtAuthenticationProvider should have
        // been called 0 times since authentication should not be done on white listed endpoints
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(jwtAuthenticationProvider, times(0)).authenticate(any());
    }

    @Test
    public void testAttemptAuthentication() {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(Endpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "goodToken")
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        Authentication authentication = jwtProcessingFilter.attemptAuthentication(request, response);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEmpty();
        assertThat(authentication.getDetails())
                .isNull();
    }
}
