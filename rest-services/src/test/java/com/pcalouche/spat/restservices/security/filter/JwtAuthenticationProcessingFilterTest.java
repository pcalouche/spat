package com.pcalouche.spat.restservices.security.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.shared.AbstractUnitTest;
import org.junit.Before;
import org.junit.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockServletContext;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class JwtAuthenticationProcessingFilterTest extends AbstractUnitTest {
    private final ObjectMapper objectMapper = new ObjectMapper();
    @MockBean
    private JwtAuthenticationProvider jwtAuthenticationProvider;
    @MockBean
    private FilterChain filterChain;
    private JwtAuthenticationProcessingFilter jwtAuthenticationProcessingFilter;

    @Before
    public void before() {
        given(jwtAuthenticationProvider.supports(JwtAuthenticationToken.class)).willCallRealMethod();

        JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("goodToken");
        given(jwtAuthenticationProvider.authenticate(jwtAuthenticationToken)).willReturn(
                new JwtAuthenticationToken("activeUser", null, Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")))
        );

        JwtAuthenticationToken jwtRefreshAuthenticationToken = new JwtAuthenticationToken("goodRefreshToken");
        jwtRefreshAuthenticationToken.setDetails("refreshToken");
        given(jwtAuthenticationProvider.authenticate(jwtRefreshAuthenticationToken)).willReturn(
                new JwtAuthenticationToken("activeUser", null, Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")))
        );

        AuthenticationManager authenticationManager = new ProviderManager(Collections.singletonList(jwtAuthenticationProvider));

        jwtAuthenticationProcessingFilter = new JwtAuthenticationProcessingFilter(authenticationManager, objectMapper);
    }

    @Test
    public void testExpectedPathsAreAuthenticated() throws IOException, ServletException {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "goodToken")
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        // Use an endpoint that is not white listed for testing
        assertThatCode(() -> jwtAuthenticationProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once along with the towerAuthenticationProvider
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(jwtAuthenticationProvider, times(1)).authenticate(any());
    }

    @Test
    public void testWhiteListedPathsAreNotAuthenticated() throws IOException, ServletException {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(SecurityUtils.WHITELISTED_ENDPOINTS[2])
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        // Use an endpoint that is white listed for testing
        assertThatCode(() -> jwtAuthenticationProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once, but the towerAuthenticationProvider should have
        // been called 0 times since authentication should not be done on white listed endpoints
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(jwtAuthenticationProvider, times(0)).authenticate(any());
    }

    @Test
    public void testAttemptAuthentication() throws Exception {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "goodToken")
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        Authentication authentication = jwtAuthenticationProcessingFilter.attemptAuthentication(request, response);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        assertThat(authentication.getDetails())
                .isNull();
    }

    @Test
    public void testAttemptAuthenticationForRefreshTokenEndpoint() throws Exception {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(SecurityUtils.REFRESH_TOKEN_ENDPOINT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "goodRefreshToken")
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());

        MockHttpServletResponse response = new MockHttpServletResponse();

        Authentication authentication = jwtAuthenticationProcessingFilter.attemptAuthentication(request, response);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        assertThat(authentication.getDetails())
                .isEqualTo("refreshToken");
    }
}
