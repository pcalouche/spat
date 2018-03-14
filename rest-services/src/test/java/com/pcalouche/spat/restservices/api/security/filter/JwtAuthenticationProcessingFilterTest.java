package com.pcalouche.spat.restservices.api.security.filter;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.api.user.controller.UserEndpoints;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = UserController.class)
public class JwtAuthenticationProcessingFilterTest extends AbstractControllerTest {
    @Test
    public void testAttemptAuthentication() {
        //TODO
    }

    @Test
    public void testAttemptAuthenticationForRefreshTokenEndpoint() {
        //TODO
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
