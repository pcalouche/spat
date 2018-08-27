package com.pcalouche.spat.restservices.security.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.security.provider.AjaxLoginAuthenticationProvider;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import com.pcalouche.spat.shared.AbstractUnitTest;
import org.junit.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.Base64;
import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;

public class AjaxLoginProcessingFilterTest extends AbstractUnitTest {
    private final ObjectMapper objectMapper = new ObjectMapper();
    @MockBean
    private AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider;

    @Test
    public void testAttemptAuthentication() {
        given(ajaxLoginAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class)).willCallRealMethod();

        UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken = new UsernamePasswordAuthenticationToken(
                "activeUser",
                "password"
        );
        given(ajaxLoginAuthenticationProvider.authenticate(usernamePasswordAuthenticationToken)).willReturn(
                new UsernamePasswordAuthenticationToken("activeUser", null, Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")))
        );

        AuthenticationManager authenticationManager = new ProviderManager(Collections.singletonList(ajaxLoginAuthenticationProvider));
        AjaxLoginProcessingFilter ajaxLoginProcessingFilter = new AjaxLoginProcessingFilter(authenticationManager, objectMapper);

        MockHttpServletRequest request = new MockHttpServletRequest(HttpMethod.GET.name(), SecurityUtils.TOKEN_ENDPOINT);
        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes()));
        MockHttpServletResponse response = new MockHttpServletResponse();

        Authentication authentication = ajaxLoginProcessingFilter.attemptAuthentication(request, response);

        assertThat(authentication.getName())
                .isEqualTo("activeUser");
        assertThat(authentication.getCredentials())
                .isNull();
        assertThat(authentication.getAuthorities())
                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
    }
}
