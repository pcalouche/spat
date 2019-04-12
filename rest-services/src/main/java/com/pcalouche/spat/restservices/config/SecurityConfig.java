package com.pcalouche.spat.restservices.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.api.repository.UserRepository;
import com.pcalouche.spat.restservices.security.filter.AjaxLoginProcessingFilter;
import com.pcalouche.spat.restservices.security.filter.JwtAuthenticationProcessingFilter;
import com.pcalouche.spat.restservices.security.provider.AjaxLoginAuthenticationProvider;
import com.pcalouche.spat.restservices.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.springframework.context.annotation.Bean;
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
    private final ObjectMapper objectMapper;
    private final AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider;
    private final JwtAuthenticationProvider jwtAuthenticationProvider;
    private final AuthenticationManager authenticationManager;

    public SecurityConfig(UserRepository userRepository,
                          ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
        ajaxLoginAuthenticationProvider = new AjaxLoginAuthenticationProvider(userRepository);
        jwtAuthenticationProvider = new JwtAuthenticationProvider(userRepository);
        authenticationManager = new ProviderManager(Arrays.asList(ajaxLoginAuthenticationProvider, jwtAuthenticationProvider));
    }

    @Override
    public AuthenticationManager authenticationManager() {
        return authenticationManager;
    }

    @Bean
    @Override
    public AuthenticationManager authenticationManagerBean() throws Exception {
        return super.authenticationManagerBean();
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        AjaxLoginProcessingFilter ajaxLoginProcessingFilter = new AjaxLoginProcessingFilter(authenticationManager);
        JwtAuthenticationProcessingFilter jwtAuthenticationProcessingFilter = new JwtAuthenticationProcessingFilter(authenticationManager, objectMapper);

        http
                // Disable basic security since we won't be using that
                .httpBasic().disable()
                // We don't need CSRF for JWT based authentication.  The nature of JWT authentication prevents CSRF.
                .csrf().disable()
                // Handle CORS (Cross-origin resource sharing)
                .cors()
                .and()
                // Need for H2 console to work properly
                .headers().frameOptions().sameOrigin()
                .and()
                // Sessions are stateless with JWT based authentication
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                // Setup which endpoints are authorized and which are not
                .and()
                .authorizeRequests()
                .antMatchers(SecurityUtils.WHITELISTED_ENDPOINTS).permitAll()
                .antMatchers(SecurityUtils.AUTHENTICATED_PATH).authenticated()
                // Setup filters for the endpoints
                .and()
                .authenticationProvider(ajaxLoginAuthenticationProvider)
                .authenticationProvider(jwtAuthenticationProvider)
                .addFilterBefore(ajaxLoginProcessingFilter, UsernamePasswordAuthenticationFilter.class)
                .addFilterBefore(jwtAuthenticationProcessingFilter, UsernamePasswordAuthenticationFilter.class);
    }
}
