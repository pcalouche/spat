package com.pcalouche.spat.config;

import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.repository.UserRepository;
import com.pcalouche.spat.security.filter.JwtProcessingFilter;
import com.pcalouche.spat.security.filter.LoginProcessingFilter;
import com.pcalouche.spat.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.security.provider.LoginAuthenticationProvider;
import com.pcalouche.spat.security.util.SecurityUtils;
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
    private final AuthenticationManager authenticationManager;

    public SecurityConfig(SecurityUtils securityUtils, UserRepository userRepository) {
        LoginAuthenticationProvider loginAuthenticationProvider = new LoginAuthenticationProvider(userRepository);
        JwtAuthenticationProvider jwtAuthenticationProvider = new JwtAuthenticationProvider(securityUtils, userRepository);
        authenticationManager = new ProviderManager(Arrays.asList(loginAuthenticationProvider, jwtAuthenticationProvider));
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
        LoginProcessingFilter loginProcessingFilter = new LoginProcessingFilter(authenticationManager);
        JwtProcessingFilter jwtProcessingFilter = new JwtProcessingFilter(authenticationManager);

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
                .antMatchers(HttpMethod.DELETE.name(), Endpoints.AUTH + Endpoints.TOKEN).permitAll()
                .antMatchers(SecurityUtils.AUTHENTICATED_PATH).authenticated()
                // Setup filters for the endpoints
                .and()
                .addFilterBefore(loginProcessingFilter, UsernamePasswordAuthenticationFilter.class)
                .addFilterBefore(jwtProcessingFilter, UsernamePasswordAuthenticationFilter.class);
    }
}
