package com.pcalouche.spat.restservices.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.security.filter.AjaxLoginProcessingFilter;
import com.pcalouche.spat.restservices.security.filter.JwtAuthenticationProcessingFilter;
import com.pcalouche.spat.restservices.security.provider.AjaxLoginAuthenticationProvider;
import com.pcalouche.spat.restservices.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
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
        authenticationManager = new ProviderManager(Arrays.asList(ajaxLoginAuthenticationProvider, jwtAuthenticationProvider));
        ajaxLoginProcessingFilter = new AjaxLoginProcessingFilter(authenticationManager, objectMapper);
        jwtAuthenticationProcessingFilter = new JwtAuthenticationProcessingFilter(authenticationManager, objectMapper);
    }

    @Bean
    @Override
    public AuthenticationManager authenticationManagerBean() {
        return authenticationManager;
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                // Disable basic security since we won't be using that
                .httpBasic().disable()
                // We don't need CSRF for JWT based authentication.  The nature JWT authentication prevents CSRF.
                .csrf().disable()
                // Need for H2 console to work properly
                .headers().frameOptions().sameOrigin()
                .and()
                // Sessions are stateless with JWT based authentication
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                // Setup which endpoints that do not require authentication
                .and()
                .authorizeRequests()
                .antMatchers(HttpMethod.GET, SecurityUtils.TOKEN_ENDPOINT).permitAll()
                .antMatchers(HttpMethod.GET, SecurityUtils.REFRESH_TOKEN_ENDPOINT).permitAll()
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
