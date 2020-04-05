package com.pcalouche.spat.restservices.security.filter;

import com.pcalouche.spat.restservices.api.Endpoints;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.restservices.util.ExceptionUtils;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class LoginProcessingFilter extends AbstractAuthenticationProcessingFilter {

    public LoginProcessingFilter(AuthenticationManager authenticationManager) {
        super(new AntPathRequestMatcher(Endpoints.AUTH + Endpoints.TOKEN, HttpMethod.POST.toString()));
        setAuthenticationManager(authenticationManager);
    }

    @Override
    public Authentication attemptAuthentication(HttpServletRequest request, HttpServletResponse response) throws AuthenticationException {
        String[] decodedBasicAuthorizationParts = SecurityUtils.getDecodedBasicAuthFromRequest(request);
        String username = decodedBasicAuthorizationParts[0];
        String password = decodedBasicAuthorizationParts[1];

        return getAuthenticationManager().authenticate(new UsernamePasswordAuthenticationToken(
                username,
                password
        ));
    }

    @Override
    protected void successfulAuthentication(HttpServletRequest request, HttpServletResponse response, FilterChain chain, Authentication authResult)
            throws IOException, ServletException {
        SecurityContextHolder.getContext().setAuthentication(authResult);
        chain.doFilter(request, response);
    }

    @Override
    protected void unsuccessfulAuthentication(HttpServletRequest request, HttpServletResponse response, AuthenticationException failed) throws IOException {
        ExceptionUtils.writeExceptionToResponse(failed, request, response);
    }
}
