package com.pcalouche.spat.restservices;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.api.entity.User;
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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

@Import({ModelMapperConfig.class, SecurityConfig.class})
@ComponentScan(basePackages = {"com.pcalouche.spat.restservices.security.provider"})
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
    protected User activeUser;
    protected User activeAdmin;
    protected User expiredUser;
    protected User lockedUser;
    protected User credentialsExpiredUser;
    protected User disabledUser;
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
        User activeUser = new User(1L, "activeUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
        activeUser.setPassword(ENCODED_PASSWORD);
        given(userService.getByUsername(activeUser.getUsername())).willReturn(activeUser);

        User activeAdmin = new User(2L, "activeAdmin", Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN")));
        activeAdmin.setPassword(ENCODED_PASSWORD);
        given(userService.getByUsername(activeAdmin.getUsername())).willReturn(activeAdmin);

        User expiredUser = new User(3L, "expiredUser", Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN")));
        expiredUser.setPassword(ENCODED_PASSWORD);
        expiredUser.setAccountNonExpired(false);
        given(userService.getByUsername(expiredUser.getUsername())).willReturn(expiredUser);

        User lockedUser = new User(4L, "lockedUser", Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN")));
        lockedUser.setPassword(ENCODED_PASSWORD);
        lockedUser.setAccountNonLocked(false);
        given(userService.getByUsername(lockedUser.getUsername())).willReturn(lockedUser);

        User credentialsExpiredUser = new User(5L, "credentialsExpiredUser", Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN")));
        credentialsExpiredUser.setPassword(ENCODED_PASSWORD);
        credentialsExpiredUser.setCredentialsNonExpired(false);
        given(userService.getByUsername(credentialsExpiredUser.getUsername())).willReturn(credentialsExpiredUser);

        User disabledUser = new User(6L, "disabledUser", Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN")));
        disabledUser.setPassword(ENCODED_PASSWORD);
        disabledUser.setEnabled(false);
        given(userService.getByUsername(disabledUser.getUsername())).willReturn(disabledUser);

        // Mock the logger interceptor
        given(loggerInterceptor.preHandle(any(), any(), any())).willReturn(true);
    }
}
