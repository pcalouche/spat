package com.pcalouche.spat.restservices.security.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.api.model.AuthResponse;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.restservices.util.ExceptionUtils;
import io.jsonwebtoken.JwtException;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class JwtAuthenticationProcessingFilter extends AbstractAuthenticationProcessingFilter {
    private final ObjectMapper objectMapper;

    public JwtAuthenticationProcessingFilter(AuthenticationManager authenticationManager, ObjectMapper objectMapper) {
        super(new JwtAuthenticationRequestMatcher());
        setAuthenticationManager(authenticationManager);
        this.objectMapper = objectMapper;
    }

    @Override
    public Authentication attemptAuthentication(HttpServletRequest request, HttpServletResponse response) throws AuthenticationException, IOException, ServletException {
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(SecurityUtils.getTokenFromRequest(request));
        if (SecurityUtils.REFRESH_TOKEN_ENDPOINT.equals(request.getRequestURI())) {
            authenticationToken.setDetails("refreshToken");
        }
        try {
            return getAuthenticationManager().authenticate(authenticationToken);
        } catch (JwtException e) {
            ExceptionUtils.writeExceptionToResponse(e, request, response);
            return null;
        }
    }

    @Override
    protected void successfulAuthentication(HttpServletRequest request, HttpServletResponse response, FilterChain chain, Authentication authResult)
            throws IOException, ServletException {
        // Include a token response if this was a refresh token request, otherwise set the SecurityContextHolder's authentication
        if (SecurityUtils.REFRESH_TOKEN_ENDPOINT.equals(request.getRequestURI())) {
            AuthResponse authResponse = SecurityUtils.createAuthResponse(authResult);
            response.setStatus(HttpStatus.OK.value());
            response.setContentType(MediaType.APPLICATION_JSON_VALUE);
            objectMapper.writeValue(response.getWriter(), authResponse);
        } else {
            SecurityContextHolder.getContext().setAuthentication(authResult);
            chain.doFilter(request, response);
        }
    }

    @Override
    protected void unsuccessfulAuthentication(HttpServletRequest request, HttpServletResponse response, AuthenticationException failed) throws IOException, ServletException {
        ExceptionUtils.writeExceptionToResponse(failed, request, response);
    }
}
