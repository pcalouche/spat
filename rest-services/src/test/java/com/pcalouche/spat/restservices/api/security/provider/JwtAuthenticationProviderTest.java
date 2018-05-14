package com.pcalouche.spat.restservices.api.security.provider;

import com.pcalouche.spat.shared.AbstractUnitTest;

public class JwtAuthenticationProviderTest extends AbstractUnitTest {
//    private JwtAuthenticationProvider jwtAuthenticationProvider;
//    @MockBean
//    private UserService userService;
//    private String validJwtToken;
//
//    @Before
//    public void before() {
//        Mockito.reset(userService);
//        User activeUser = new User(1L, "activeUser", Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
//        activeUser.setPassword("$2a$10$VSkAHLuuGgU.Oo/5TpiKieHSdW2Whz83PfPJoFvvrh.pQbT2YsNSi");
//        given(userService.findByUsername(activeUser.getUsername())).willReturn(activeUser);
//
//        given(userService.findByUsername("bogusUser")).willReturn(null);
//
//        jwtAuthenticationProvider = new JwtAuthenticationProvider(userService);
//
//        List<SimpleGrantedAuthority> authorities = Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER"));
//        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken("activeUser", "pretendToken", authorities);
//        AuthResponseDto authResponseDto = SecurityUtils.createAuthResponse(authenticationToken);
//        validJwtToken = authResponseDto.getToken();
//    }
//
//    @Test
//    public void testAuthenticate() {
//        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validJwtToken);
//
//        Authentication authentication = jwtAuthenticationProvider.authenticate(authenticationToken);
//
//        assertThat(authentication.getName())
//                .isEqualTo("activeUser");
//        assertThat(authentication.getCredentials())
//                .isNull();
//        assertThat(authentication.getAuthorities())
//                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
//
//        // User service should not be hit for non refresh token
//        verify(userService, Mockito.times(0)).findByUsername("activeUser");
//    }
//
//    @Test
//    public void testAuthenticateThrowsBadCredentialsForNullToken() {
//        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(null);
//
//        assertThatThrownBy(() -> jwtAuthenticationProvider.authenticate(authenticationToken))
//                .isInstanceOf(BadCredentialsException.class)
//                .hasMessage("JSON web token was empty.");
//    }
//
//    @Test
//    public void testAuthenticateThrowsHandlesRefreshToken() {
//        JwtAuthenticationToken authenticationToken = new JwtAuthenticationToken(validJwtToken);
//        authenticationToken.setDetails("refreshToken");
//
//        Authentication authentication = jwtAuthenticationProvider.authenticate(authenticationToken);
//
//        assertThat(authentication.getName())
//                .isEqualTo("activeUser");
//        assertThat(authentication.getCredentials())
//                .isNull();
//        assertThat(authentication.getAuthorities())
//                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
//
//        // User service should be hit for non refresh token
//        verify(userService, Mockito.times(1)).findByUsername("activeUser");
//    }
//
//    @Test
//    public void testSupportValidationAuthenticationClass() {
//        assertThat(jwtAuthenticationProvider.supports(JwtAuthenticationToken.class))
//                .isTrue();
//    }
//
//    @Test
//    public void testSupportInValidationAuthenticationClass() {
//        assertThat(jwtAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class))
//                .isFalse();
//    }
}
