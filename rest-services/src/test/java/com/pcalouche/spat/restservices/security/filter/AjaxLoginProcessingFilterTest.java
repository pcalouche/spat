package com.pcalouche.spat.restservices.security.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.security.provider.AjaxLoginAuthenticationProvider;
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
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.Base64;
import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class AjaxLoginProcessingFilterTest extends AbstractUnitTest {
    private final ObjectMapper objectMapper = new ObjectMapper();
    @MockBean
    private AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider;
    @MockBean
    private FilterChain filterChain;
    private AjaxLoginProcessingFilter ajaxLoginProcessingFilter;

    @Before
    public void before() {
        given(ajaxLoginAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class)).willCallRealMethod();
        UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken = new UsernamePasswordAuthenticationToken(
                "activeUser",
                "password"
        );
        given(ajaxLoginAuthenticationProvider.authenticate(usernamePasswordAuthenticationToken)).willReturn(
                new UsernamePasswordAuthenticationToken("activeUser", null, Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")))
        );

        AuthenticationManager authenticationManager = new ProviderManager(Collections.singletonList(ajaxLoginAuthenticationProvider));

        ajaxLoginProcessingFilter = new AjaxLoginProcessingFilter(authenticationManager, objectMapper);
    }

    @Test
    public void testExpectedPathsAreAuthenticated() throws IOException, ServletException {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(SecurityUtils.TOKEN_ENDPOINT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes()))
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        // Use an endpoint that is not white listed for testing
        assertThatCode(() -> ajaxLoginProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once along with the towerAuthenticationProvider
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(ajaxLoginAuthenticationProvider, times(1)).authenticate(any());
    }

    @Test
    public void testWhiteListedPathsAreNotAuthenticated() throws IOException, ServletException {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(SecurityUtils.WHITELISTED_ENDPOINTS[1])
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        // Use an endpoint that is white listed for testing
        assertThatCode(() -> ajaxLoginProcessingFilter.doFilter(request, response, filterChain))
                .doesNotThrowAnyException();

        // The filter chain should have been called once, but the towerAuthenticationProvider should have
        // been called 0 times since authentication should not be done on white listed endpoints
        verify(filterChain, times(1)).doFilter(any(), any());
        verify(ajaxLoginAuthenticationProvider, times(0)).authenticate(any());
    }

    @Test
    public void testAttemptAuthentication() {
        MockHttpServletRequest request = MockMvcRequestBuilders.get(SecurityUtils.TOKEN_ENDPOINT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes()))
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        MockHttpServletResponse response = new MockHttpServletResponse();

        Authentication authentication = ajaxLoginProcessingFilter.attemptAuthentication(request, response);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
    }
}
