package com.pcalouche.spat.security.filter;

import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.security.util.SecurityUtils;
import com.pcalouche.spat.util.ExceptionUtils;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;
import org.springframework.security.web.util.matcher.AndRequestMatcher;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.security.web.util.matcher.NegatedRequestMatcher;
import org.springframework.security.web.util.matcher.OrRequestMatcher;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Arrays;
import java.util.stream.Collectors;

public class JwtProcessingFilter extends AbstractAuthenticationProcessingFilter {

    public JwtProcessingFilter(AuthenticationManager authenticationManager) {
        // This matcher includes all authenticated paths that require a JWT token while excluding
        // whitelisted URLS like Swagger URLs
        super(new AndRequestMatcher(
                new AntPathRequestMatcher(SecurityUtils.AUTHENTICATED_PATH),
                new NegatedRequestMatcher(new AntPathRequestMatcher(Endpoints.AUTH + Endpoints.TOKEN, HttpMethod.POST.toString())),
                new NegatedRequestMatcher(new OrRequestMatcher(
                        Arrays.stream(SecurityUtils.WHITELISTED_ENDPOINTS)
                                .map(AntPathRequestMatcher::new)
                                .collect(Collectors.toList()))
                ))
        );
        setAuthenticationManager(authenticationManager);
    }

    @Override
    public Authentication attemptAuthentication(HttpServletRequest request, HttpServletResponse response) throws AuthenticationException {
        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(SecurityUtils.getTokenFromRequest(request));
        return getAuthenticationManager().authenticate(authenticationToken);
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
