package com.pcalouche.spat.restservices.api.security.filter;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.api.user.controller.UserEndpoints;
import com.pcalouche.spat.restservices.security.filter.JwtAuthenticationProcessingFilter;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = UserController.class)
public class JwtAuthenticationProcessingFilterTest extends AbstractControllerTest {
    @Autowired
    private AuthenticationManager authenticationManager;

    @Test
    public void testAttemptAuthentication() throws Exception {
        JwtAuthenticationProcessingFilter jwtAuthenticationProcessingFilter = new JwtAuthenticationProcessingFilter(authenticationManager, objectMapper);
        MockHttpServletRequest request = new MockHttpServletRequest(HttpMethod.GET.name(), UserEndpoints.ROOT);
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + getValidUserToken());
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
        JwtAuthenticationProcessingFilter jwtAuthenticationProcessingFilter = new JwtAuthenticationProcessingFilter(authenticationManager, objectMapper);
        MockHttpServletRequest request = new MockHttpServletRequest(HttpMethod.GET.name(), SecurityUtils.REFRESH_TOKEN_ENDPOINT);
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + getValidUserToken());
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

    @Test
    public void testSuccessfulAuthentication() throws Exception {
        mockMvc.perform(get(UserEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + getValidUserToken()))
                .andExpect(status().isOk());
    }

    @Test
    public void testSuccessfulAuthenticationForRefreshTokenEndpoint() throws Exception {
        mockMvc.perform(get(SecurityUtils.REFRESH_TOKEN_ENDPOINT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").exists())
                .andExpect(jsonPath("$.refreshToken").exists());
    }

    @Test
    public void testUnsuccessfulAuthentication() throws Exception {
        mockMvc.perform(get(UserEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "badToken"))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.timestamp").exists())
                .andExpect(jsonPath("$.path").exists())
                .andExpect(jsonPath("$.status").exists())
                .andExpect(jsonPath("$.message").exists())
                .andExpect(jsonPath("$.clientCode").exists());
    }
}
