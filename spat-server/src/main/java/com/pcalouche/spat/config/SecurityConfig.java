package com.pcalouche.spat.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.security.filter.AjaxLoginProcessingFilter;
import com.pcalouche.spat.security.filter.JwtAuthenticationProcessingFilter;
import com.pcalouche.spat.security.provider.AjaxLoginAuthenticationProvider;
import com.pcalouche.spat.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import java.util.Arrays;

@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
public class SecurityConfig extends WebSecurityConfigurerAdapter {
    private final AjaxLoginProcessingFilter ajaxLoginProcessingFilter;
    private final JwtAuthenticationProcessingFilter jwtAuthenticationProcessingFilter;
    private final AuthenticationManager authenticationManager;

    public SecurityConfig(AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider,
                          JwtAuthenticationProvider jwtAuthenticationProvider,
                          ObjectMapper objectMapper) {
        this.authenticationManager = new ProviderManager(Arrays.asList(ajaxLoginAuthenticationProvider, jwtAuthenticationProvider));
        this.ajaxLoginProcessingFilter = new AjaxLoginProcessingFilter(authenticationManager, objectMapper);
        this.jwtAuthenticationProcessingFilter = new JwtAuthenticationProcessingFilter(authenticationManager, objectMapper);
    }

    @Override
    public AuthenticationManager authenticationManager() {
        return authenticationManager;
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                // We don't need CSRF for JWT based authentication.  The nature JWT authentication prevents CSRF.
                .csrf().disable()
                // Sessions are stateless with JWT based authentication
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                // Setup which endpoints that do not require authentication
                .and()
                .authorizeRequests()
                //                .antMatchers(SecurityUtils.LOGIN_PATH).permitAll()
                .antMatchers(SecurityUtils.LOGIN_PATH, SecurityUtils.REFRESH_TOKEN_PATH).permitAll()
                //                .antMatchers(HttpMethod.POST, SecurityUtils.LOGIN_PATH).permitAll()
                //                .antMatchers(HttpMethod.GET, SecurityUtils.REFRESH_TOKEN_PATH).permitAll()
                // Setup which endpoints that do require authentication
                .and()
                .authorizeRequests()
                .antMatchers(SecurityUtils.AUTHENTICATED_PATH).authenticated()
                // Setup filters for the endpoints
                .and()
                .addFilterBefore(ajaxLoginProcessingFilter, UsernamePasswordAuthenticationFilter.class)
                .addFilterBefore(jwtAuthenticationProcessingFilter, UsernamePasswordAuthenticationFilter.class);
    }
}
