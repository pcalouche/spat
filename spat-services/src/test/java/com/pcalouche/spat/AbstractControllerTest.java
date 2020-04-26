package com.pcalouche.spat;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.config.SecurityConfig;
import com.pcalouche.spat.config.SpatProperties;
import com.pcalouche.spat.interceptors.LoggerInterceptor;
import com.pcalouche.spat.repository.UserRepository;
import com.pcalouche.spat.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.security.util.SecurityUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

@ExtendWith(SpringExtension.class)
@Import({
        SpatProperties.class,
        SecurityUtils.class,
        SecurityConfig.class
})
public abstract class AbstractControllerTest {
    @Autowired
    protected ObjectMapper objectMapper;
    @Autowired
    protected MockMvc mockMvc;
    @Autowired
    protected SecurityUtils securityUtils;
    @Autowired
    protected SpatProperties spatProperties;
    @MockBean
    protected LoggerInterceptor loggerInterceptor;
    @MockBean
    protected UserRepository userRepository;
    private String validUserToken;
    private String validAdminToken;

    protected String getValidUserToken() {
        if (validUserToken == null) {
            JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("activeUser", new HashSet<>());
            validUserToken = SecurityUtils.AUTH_HEADER_BEARER_PREFIX + securityUtils.createToken(jwtAuthenticationToken);
        }
        return validUserToken;
    }

    protected String getValidAdminToken() {
        if (validAdminToken == null) {
            Set<SimpleGrantedAuthority> authorities = Stream.of(new SimpleGrantedAuthority("Admin")).collect(Collectors.toSet());
            JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("activeAdmin", authorities);
            validAdminToken = SecurityUtils.AUTH_HEADER_BEARER_PREFIX + securityUtils.createToken(jwtAuthenticationToken);
        }
        return validAdminToken;
    }

    @BeforeEach
    public void setup() {
        // Mock the logger interceptor
        given(loggerInterceptor.preHandle(any(), any(), any())).willReturn(true);
    }
}
