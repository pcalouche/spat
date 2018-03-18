package com.pcalouche.spat.restservices.api.security.filter;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.security.filter.AjaxLoginProcessingFilter;
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

import java.util.Base64;
import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = UserController.class)
public class AjaxLoginProcessingFilterTest extends AbstractControllerTest {
    @Autowired
    private AuthenticationManager authenticationManager;

    @Test
    public void testAttemptAuthentication() {
        AjaxLoginProcessingFilter ajaxLoginProcessingFilter = new AjaxLoginProcessingFilter(authenticationManager, objectMapper);
        MockHttpServletRequest request = new MockHttpServletRequest(HttpMethod.GET.name(), SecurityUtils.TOKEN_ENDPOINT);
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes()));
        MockHttpServletResponse response = new MockHttpServletResponse();

        Authentication authentication = ajaxLoginProcessingFilter.attemptAuthentication(request, response);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
    }

    @Test
    public void testSuccessfulAuthentication() throws Exception {
        mockMvc.perform(get(SecurityUtils.TOKEN_ENDPOINT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").exists())
                .andExpect(jsonPath("$.refreshToken").exists());
    }

    @Test
    public void testUnsuccessfulAuthentication() throws Exception {
        mockMvc.perform(get(SecurityUtils.TOKEN_ENDPOINT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:badPassword".getBytes())))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.timestamp").exists())
                .andExpect(jsonPath("$.path").exists())
                .andExpect(jsonPath("$.status").exists())
                .andExpect(jsonPath("$.message").exists())
                .andExpect(jsonPath("$.clientCode").exists());
    }
}
