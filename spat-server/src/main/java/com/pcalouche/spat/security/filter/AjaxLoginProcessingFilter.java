package com.pcalouche.spat.security.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.api.model.AuthRequest;
import com.pcalouche.spat.api.model.AuthResponse;
import com.pcalouche.spat.security.util.SecurityUtils;
import com.pcalouche.spat.util.ExceptionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;
import org.springframework.stereotype.Component;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@Component
public class AjaxLoginProcessingFilter extends AbstractAuthenticationProcessingFilter {
    private final ObjectMapper objectMapper;

    public AjaxLoginProcessingFilter(AuthenticationManager authenticationManager, ObjectMapper objectMapper) {
        super(SecurityUtils.LOGIN_PATH);
        this.setAuthenticationManager(authenticationManager);
        this.objectMapper = objectMapper;
    }

    @Override
    public Authentication attemptAuthentication(HttpServletRequest request, HttpServletResponse response) throws AuthenticationException, IOException, ServletException {
        AuthRequest authRequest = objectMapper.readValue(request.getReader(), AuthRequest.class);

        return this.getAuthenticationManager().authenticate(new UsernamePasswordAuthenticationToken(
                authRequest.getUsername(),
                authRequest.getPassword())
        );
    }

    @Override
    protected void successfulAuthentication(HttpServletRequest request, HttpServletResponse response, FilterChain chain, Authentication authResult)
            throws IOException, ServletException {
        AuthResponse authResponse = SecurityUtils.createAuthResponse(authResult);
        response.setStatus(HttpStatus.OK.value());
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        objectMapper.writeValue(response.getWriter(), authResponse);
    }

    @Override
    protected void unsuccessfulAuthentication(HttpServletRequest request, HttpServletResponse response, AuthenticationException failed) throws IOException, ServletException {
        ExceptionUtils.writeExceptionToResponse(failed, response);
    }
}
