package com.pcalouche.spat.restservices;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import com.pcalouche.spat.restservices.api.user.repository.UserRepository;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import com.pcalouche.spat.restservices.config.ModelMapperConfig;
import com.pcalouche.spat.restservices.config.SecurityConfig;
import com.pcalouche.spat.restservices.interceptors.LoggerInterceptor;
import com.pcalouche.spat.restservices.security.authentication.JwtAuthenticationToken;
import com.pcalouche.spat.restservices.security.provider.AjaxLoginAuthenticationProvider;
import com.pcalouche.spat.restservices.security.provider.JwtAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.shared.AbstractUnitTest;
import org.junit.Before;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Import;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.web.servlet.MockMvc;

import javax.persistence.EntityManagerFactory;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

@Import({ModelMapperConfig.class, SecurityConfig.class})
@ComponentScan(basePackages = {"com.pcalouche.spat.restservices.security.provider"})  //TODO is this needed?
public abstract class AbstractControllerTest extends AbstractUnitTest {
    private final static String ENCODED_PASSWORD = "$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi";
    @Autowired
    protected ObjectMapper objectMapper;
    @Autowired
    protected MockMvc mockMvc;
    @Autowired
    protected ModelMapper modelMapper;
    @MockBean
    protected LoggerInterceptor loggerInterceptor;
    @Autowired
    protected AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider;
    @Autowired
    protected JwtAuthenticationProvider jwtAuthenticationProvider;
    @MockBean
    protected UserService userService;
    @MockBean
    protected UserRepository userRepository;
    @MockBean
    protected EntityManagerFactory entityManagerFactory;
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
        Set<Role> userRoles = new HashSet<>();
        Role userRole = new Role("ROLE_USER");
        userRole.setId(1L);
        userRoles.add(new Role("ROLE_USER"));

        Role adminRole = new Role("ROLE_ADMIN");
        adminRole.setId(2L);
        Set<Role> adminRoles = new HashSet<>(userRoles);
        adminRoles.add(adminRole);
        // Setup some mocks for the user service can be used by other tests
        User activeUser = new User(1L, "activeUser", userRoles);
        activeUser.setPassword(ENCODED_PASSWORD);
        given(userRepository.findByUsername(activeUser.getUsername())).willReturn(activeUser);

        User activeAdmin = new User(2L, "activeAdmin", adminRoles);
        activeAdmin.setPassword(ENCODED_PASSWORD);
        given(userRepository.findByUsername(activeAdmin.getUsername())).willReturn(activeAdmin);

        User expiredUser = new User(3L, "expiredUser", adminRoles);
        expiredUser.setPassword(ENCODED_PASSWORD);
        expiredUser.setAccountNonExpired(false);
        given(userRepository.findByUsername(expiredUser.getUsername())).willReturn(expiredUser);

        User lockedUser = new User(4L, "lockedUser", adminRoles);
        lockedUser.setPassword(ENCODED_PASSWORD);
        lockedUser.setAccountNonLocked(false);
        given(userRepository.findByUsername(lockedUser.getUsername())).willReturn(lockedUser);

        User credentialsExpiredUser = new User(5L, "credentialsExpiredUser", adminRoles);
        credentialsExpiredUser.setPassword(ENCODED_PASSWORD);
        credentialsExpiredUser.setCredentialsNonExpired(false);
        given(userRepository.findByUsername(credentialsExpiredUser.getUsername())).willReturn(credentialsExpiredUser);

        User disabledUser = new User(6L, "disabledUser", adminRoles);
        disabledUser.setPassword(ENCODED_PASSWORD);
        disabledUser.setEnabled(false);
        given(userRepository.findByUsername(disabledUser.getUsername())).willReturn(disabledUser);

        // Mock the logger interceptor
        given(loggerInterceptor.preHandle(any(), any(), any())).willReturn(true);
    }
}
