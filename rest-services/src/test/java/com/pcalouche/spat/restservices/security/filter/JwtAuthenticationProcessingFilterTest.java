package com.pcalouche.spat.restservices.security.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.shared.AbstractUnitTest;
import org.junit.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;

public class JwtAuthenticationProcessingFilterTest extends AbstractUnitTest {
    private final ObjectMapper objectMapper = new ObjectMapper();
    @MockBean
    private JwtAuthenticationProvider jwtAuthenticationProvider;

    @Test
    public void testAttemptAuthentication() throws Exception {
        given(jwtAuthenticationProvider.supports(JwtAuthenticationToken.class)).willCallRealMethod();

        JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("goodToken");
        given(jwtAuthenticationProvider.authenticate(jwtAuthenticationToken)).willReturn(
                new JwtAuthenticationToken("activeUser", null, Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")))
        );

        AuthenticationManager authenticationManager = new ProviderManager(Collections.singletonList(jwtAuthenticationProvider));
        JwtAuthenticationProcessingFilter jwtAuthenticationProcessingFilter = new JwtAuthenticationProcessingFilter(authenticationManager, objectMapper);

        MockHttpServletRequest request = new MockHttpServletRequest(HttpMethod.GET.name(), ApiEndpoints.USER);
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "goodToken");
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
        given(jwtAuthenticationProvider.supports(JwtAuthenticationToken.class)).willCallRealMethod();

        JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("goodToken");
        jwtAuthenticationToken.setDetails("refreshToken");
        given(jwtAuthenticationProvider.authenticate(jwtAuthenticationToken)).willReturn(
                new JwtAuthenticationToken("activeUser", null, Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")))
        );

        AuthenticationManager authenticationManager = new ProviderManager(Collections.singletonList(jwtAuthenticationProvider));
        JwtAuthenticationProcessingFilter jwtAuthenticationProcessingFilter = new JwtAuthenticationProcessingFilter(authenticationManager, objectMapper);

        MockHttpServletRequest request = new MockHttpServletRequest(HttpMethod.GET.name(), SecurityUtils.REFRESH_TOKEN_ENDPOINT);
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "goodToken");
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
