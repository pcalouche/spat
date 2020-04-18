package com.pcalouche.spat.api.controller;

import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.entity.User;
import com.pcalouche.spat.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;

import java.util.Base64;
import java.util.HashSet;
import java.util.Optional;

import static org.hamcrest.Matchers.is;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(AuthController.class)
public class AuthControllerTest extends AbstractControllerTest {
    @Test
    public void testToken() throws Exception {
        User existingUser = User.builder()
                .id(1)
                .username("activeUser")
                .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                .build();

        String basicAuthValue = new String(Base64.getEncoder().encode(("activeUser:password").getBytes()));

        given(userRepository.findByUsername("activeUser")).willReturn(Optional.of(existingUser));

        mockMvc.perform(post(Endpoints.AUTH + Endpoints.TOKEN)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + basicAuthValue))
                .andExpect(status().isOk());
    }

    @Test
    public void testRefreshToken() throws Exception {
        JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("activeUser", "pretendToken", new HashSet<>());
        String validAdminRefreshToken = SecurityUtils.AUTH_HEADER_BEARER_PREFIX + securityUtils.createAuthResponse(jwtAuthenticationToken).getRefreshToken();

        User existingUser = User.builder()
                .id(1)
                .username("activeUser")
                .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                .build();

        given(userRepository.findByUsername("activeUser")).willReturn(Optional.of(existingUser));

        mockMvc.perform(post(Endpoints.AUTH + Endpoints.REFRESH_TOKEN)
                .header(HttpHeaders.AUTHORIZATION, validAdminRefreshToken))
                .andExpect(status().isOk());
    }

    @Test
    public void testRefreshTokenExpectsARefreshToken() throws Exception {
        mockMvc.perform(post(Endpoints.AUTH + Endpoints.REFRESH_TOKEN)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.message", is("Non refresh token used")));
    }
}
