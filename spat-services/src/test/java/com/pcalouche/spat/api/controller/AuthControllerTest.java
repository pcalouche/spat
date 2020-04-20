package com.pcalouche.spat.api.controller;

import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.entity.User;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;

import javax.servlet.http.Cookie;
import java.util.Base64;
import java.util.Optional;

import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

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
                .andExpect(status().isOk())
                .andExpect(cookie().exists("refresh_token"))
                .andExpect(cookie().path("refresh_token", "/"))
                .andExpect(cookie().domain("refresh_token", spatProperties.getHostname()))
                .andExpect(cookie().httpOnly("refresh_token", true))
                .andExpect(cookie().secure("refresh_token", spatProperties.isHttpsEnvironment()))
                .andExpect(jsonPath("$").exists());
    }

    @Test
    public void testDeleteToken() throws Exception {
        mockMvc.perform(delete(Endpoints.AUTH + Endpoints.TOKEN))
                .andExpect(header().exists(HttpHeaders.SET_COOKIE))
                .andExpect(status().isOk())
                .andExpect(cookie().value("refresh_token", ""))
                .andExpect(cookie().path("refresh_token", "/"))
                .andExpect(cookie().domain("refresh_token", spatProperties.getHostname()))
                .andExpect(cookie().httpOnly("refresh_token", true))
                .andExpect(cookie().secure("refresh_token", spatProperties.isHttpsEnvironment()))
                .andExpect(cookie().maxAge("refresh_token", 0))
                .andReturn();
    }

    @Test
    public void testRefreshToken() throws Exception {
        User existingUser = User.builder()
                .id(1)
                .username("activeUser")
                .password(SecurityUtils.PASSWORD_ENCODER.encode("password"))
                .build();

        given(userRepository.findByUsername("activeUser")).willReturn(Optional.of(existingUser));

        Cookie refreshTokenCookie = new Cookie("refresh_token", securityUtils.createRefreshTokenCookie("activeUser").getValue());

        mockMvc.perform(post(Endpoints.AUTH + Endpoints.REFRESH_TOKEN)
                .cookie(refreshTokenCookie))
                .andExpect(status().isOk())
                .andExpect(cookie().exists("refresh_token"))
                .andExpect(cookie().path("refresh_token", "/"))
                .andExpect(cookie().domain("refresh_token", spatProperties.getHostname()))
                .andExpect(cookie().httpOnly("refresh_token", true))
                .andExpect(cookie().secure("refresh_token", spatProperties.isHttpsEnvironment()));
    }
}
