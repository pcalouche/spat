package com.pcalouche.spat.restservices;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.config.SecurityConfig;
import com.pcalouche.spat.restservices.entity.Role;
import com.pcalouche.spat.restservices.entity.User;
import com.pcalouche.spat.restservices.interceptors.LoggerInterceptor;
import com.pcalouche.spat.restservices.repository.UserRepository;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.junit.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.web.servlet.MockMvc;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

@Import({SecurityConfig.class})
public abstract class AbstractControllerTest extends AbstractTest {
    private final static String ENCODED_PASSWORD = "$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi";
    @Autowired
    protected ObjectMapper objectMapper;
    @Autowired
    protected MockMvc mockMvc;
    @MockBean
    protected LoggerInterceptor loggerInterceptor;
    @MockBean
    protected UserRepository userRepository;
    private String validUserToken;
    private String validAdminToken;

    protected String getValidUserToken() {
        if (validUserToken == null) {
            JwtAuthenticationToken jwtAuthenticationToken = new JwtAuthenticationToken("activeUser", "pretendToken", new HashSet<>());
            validUserToken = SecurityUtils.AUTH_HEADER_BEARER_PREFIX + SecurityUtils.createAuthResponse(jwtAuthenticationToken).getToken();
        }
        return validUserToken;
    }

    protected String getValidAdminToken() {
        if (validAdminToken == null) {
            Set<SimpleGrantedAuthority> authorities = Stream.of(new SimpleGrantedAuthority("Admin")).collect(Collectors.toSet());
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
                .id(1)
                .name("USER")
                .build());

        Set<Role> adminRoles = new HashSet<>(userRoles);
        adminRoles.add(Role.builder()
                .id(2)
                .name("Admin")
                .build());

        User activeUser = User.builder()
                .username("activeUser")
                .password(ENCODED_PASSWORD)
                .roles(userRoles)
                .build();
        given(userRepository.getOne(activeUser.getUsername())).willReturn(activeUser);

        User activeAdmin = User.builder()
                .username("activeAdmin")
                .password(ENCODED_PASSWORD)
                .roles(adminRoles)
                .build();
        given(userRepository.getOne(activeAdmin.getUsername())).willReturn(activeAdmin);

        User expiredUser = User.builder()
                .username("expiredUser")
                .password(ENCODED_PASSWORD)
                .accountNonExpired(false)
                .roles(adminRoles)
                .build();
        given(userRepository.getOne(expiredUser.getUsername())).willReturn(expiredUser);

        User lockedUser = User.builder()
                .username("lockedUser")
                .password(ENCODED_PASSWORD)
                .accountNonLocked(false)
                .roles(adminRoles)
                .build();
        lockedUser.setAccountNonLocked(false);
        given(userRepository.getOne(lockedUser.getUsername())).willReturn(lockedUser);

        User credentialsExpiredUser = User.builder()
                .username("credentialsExpiredUser")
                .password(ENCODED_PASSWORD)
                .credentialsNonExpired(false)
                .roles(adminRoles)
                .build();
        given(userRepository.getOne(credentialsExpiredUser.getUsername())).willReturn(credentialsExpiredUser);

        User disabledUser = User.builder()
                .username("disabledUser")
                .password(ENCODED_PASSWORD)
                .enabled(false)
                .roles(adminRoles)
                .build();
        given(userRepository.getOne(disabledUser.getUsername())).willReturn(disabledUser);

        // Mock the logger interceptor
        given(loggerInterceptor.preHandle(any(), any(), any())).willReturn(true);
    }
}
