package com.pcalouche.spat.restservices.api.security.filter;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.security.util.SecurityUtils;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;

import java.util.Base64;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = UserController.class)
public class AjaxLoginProcessingFilterTest extends AbstractControllerTest {
    //    @Autowired
    //    private AjaxLoginProcessingFilter ajaxLoginProcessingFilter;
    //
    //    @Test
    //    public void testAttemptAuthentication() {
    //        MockHttpServletRequest request = new MockHttpServletRequest(HttpMethod.GET.name(), SecurityUtils.TOKEN_ENDPOINT);
    //        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes()));
    //        MockHttpServletResponse response = new MockHttpServletResponse();
    //
    //        Authentication authentication = ajaxLoginProcessingFilter.attemptAuthentication(request, response);
    //        assertThat(authentication.getName())
    //                .isEqualTo("activeUser");
    //        assertThat(authentication.getCredentials())
    //                .isNull();
    //        assertThat(authentication.getAuthorities())
    //                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
    //    }
    //
    //    @Test
    //    public void testAttemptAuthenticationThrowsAuthenticationException() {
    //        MockHttpServletRequest request = new MockHttpServletRequest(HttpMethod.GET.name(), SecurityUtils.TOKEN_ENDPOINT);
    //        request.addHeader(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:badPassword".getBytes()));
    //        MockHttpServletResponse response = new MockHttpServletResponse();
    //
    //        assertThatThrownBy(() -> ajaxLoginProcessingFilter.attemptAuthentication(request, response))
    //                .isInstanceOf(BadCredentialsException.class)
    //                .hasMessage("Bad credentials for username: activeUser");
    //    }

    @Test
    public void testSuccessfulAuthentication() throws Exception {
        mockMvc.perform(get(SecurityUtils.TOKEN_ENDPOINT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:password".getBytes())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").exists())
                .andExpect(jsonPath("$.refreshToken").exists());
    }

    @Test
    public void testUnsuccessfulAuthentication() throws Exception {
        mockMvc.perform(get(SecurityUtils.TOKEN_ENDPOINT)
                .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BASIC_PREFIX + Base64.getEncoder().encodeToString("activeUser:badPassword".getBytes())))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.timestamp").exists())
                .andExpect(jsonPath("$.path").exists())
                .andExpect(jsonPath("$.status").exists())
                .andExpect(jsonPath("$.message").exists())
                .andExpect(jsonPath("$.clientCode").exists());
    }
}
