package com.pcalouche.spat.restservices;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.repository.UserRepository;
import com.pcalouche.spat.restservices.config.SecurityConfig;
import com.pcalouche.spat.restservices.interceptors.LoggerInterceptor;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.provider.AjaxLoginAuthenticationProvider;
import com.pcalouche.spat.restservices.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.shared.AbstractUnitTest;
import org.junit.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Import;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.web.servlet.MockMvc;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

@Import({SecurityConfig.class})
@ComponentScan(basePackages = {"com.pcalouche.spat.restservices.security"})
public abstract class AbstractControllerTest extends AbstractUnitTest {
    private final static String ENCODED_PASSWORD = "$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi";
    @Autowired
    protected ObjectMapper objectMapper;
    @Autowired
    protected MockMvc mockMvc;
    @MockBean
    protected LoggerInterceptor loggerInterceptor;
    @Autowired
    protected AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider;
    @Autowired
    protected JwtAuthenticationProvider jwtAuthenticationProvider;
    @MockBean
    protected UserRepository userRepository;
    private String validUserToken;
    private String validAdminToken;

    protected String getValidUserToken() {
        if (validUserToken == null) {
            List<SimpleGrantedAuthority> authorities = Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER"));
            JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("activeUser", "pretendToken", authorities);
            validUserToken = SecurityUtils.AUTH_HEADER_BEARER_PREFIX + SecurityUtils.createAuthResponse(jwtAuthenticationToken).getToken();
        }
        return validUserToken;
    }

    protected String getValidAdminToken() {
        if (validAdminToken == null) {
            List<SimpleGrantedAuthority> authorities = Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
            JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("activeAdmin", "pretendToken", authorities);
            validAdminToken = SecurityUtils.AUTH_HEADER_BEARER_PREFIX + SecurityUtils.createAuthResponse(jwtAuthenticationToken).getToken();
        }
        return validAdminToken;
    }

    @Before
    public void setup() {
        // Setup some mocks for the user service can be used by other tests
        Set<Role> userRoles = new HashSet<>();
        userRoles.add(Role.builder()
                .id(1L)
                .name("ROLE_USER")
                .build());

        Set<Role> adminRoles = new HashSet<>(userRoles);
        adminRoles.add(Role.builder()
                .id(2L)
                .name("ROLE_ADMIN")
                .build());

        User activeUser = User.builder()
                .id(1L)
                .username("activeUser")
                .password(ENCODED_PASSWORD)
                .roles(userRoles)
                .build();
        given(userRepository.findByUsername(activeUser.getUsername())).willReturn(activeUser);

        User activeAdmin = User.builder()
                .id(2L)
                .username("activeAdmin")
                .password(ENCODED_PASSWORD)
                .roles(adminRoles)
                .build();
        given(userRepository.findByUsername(activeAdmin.getUsername())).willReturn(activeAdmin);

        User expiredUser = User.builder()
                .id(3L)
                .username("expiredUser")
                .password(ENCODED_PASSWORD)
                .accountNonExpired(false)
                .roles(adminRoles)
                .build();
        given(userRepository.findByUsername(expiredUser.getUsername())).willReturn(expiredUser);

        User lockedUser = User.builder()
                .id(4L)
                .username("lockedUser")
                .password(ENCODED_PASSWORD)
                .accountNonLocked(false)
                .roles(adminRoles)
                .build();
        lockedUser.setAccountNonLocked(false);
        given(userRepository.findByUsername(lockedUser.getUsername())).willReturn(lockedUser);

        User credentialsExpiredUser = User.builder()
                .id(5L)
                .username("credentialsExpiredUser")
                .password(ENCODED_PASSWORD)
                .credentialsNonExpired(false)
                .roles(adminRoles)
                .build();
        given(userRepository.findByUsername(credentialsExpiredUser.getUsername())).willReturn(credentialsExpiredUser);

        User disabledUser = User.builder()
                .id(6L)
                .username("disabledUser")
                .password(ENCODED_PASSWORD)
                .enabled(false)
                .roles(adminRoles)
                .build();
        given(userRepository.findByUsername(disabledUser.getUsername())).willReturn(disabledUser);

        // Mock the logger interceptor
        given(loggerInterceptor.preHandle(any(), any(), any())).willReturn(true);
    }
}
