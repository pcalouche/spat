package com.pcalouche.spat.restservices.api.security.provider;

import com.pcalouche.spat.shared.AbstractUnitTest;

public class AjaxLoginAuthenticationProviderTest extends AbstractUnitTest {
//    private AjaxLoginAuthenticationProvider ajaxLoginAuthenticationProvider;
//    @MockBean
//    private UserService userService;
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
//        ajaxLoginAuthenticationProvider = new AjaxLoginAuthenticationProvider(userService);
//    }
//
//    @Test
//    public void testAuthenticate() {
//        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeUser", "password");
//        Authentication authentication = ajaxLoginAuthenticationProvider.authenticate(authenticationToken);
//
//        assertThat(authentication.getName())
//                .isEqualTo("activeUser");
//        assertThat(authentication.getCredentials())
//                .isNull();
//        assertThat(authentication.getAuthorities())
//                .isEqualTo(Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));
//
//        verify(userService, Mockito.times(1)).findByUsername("activeUser");
//    }
//
//    @Test
//    public void testAuthenticateThrowsBadCredentialsForNullUser() {
//        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("fakeUser", "password");
//
//        assertThatThrownBy(() -> ajaxLoginAuthenticationProvider.authenticate(authenticationToken))
//                .isInstanceOf(BadCredentialsException.class)
//                .hasMessage("Bad credentials for username: fakeUser");
//
//        verify(userService, Mockito.times(1)).findByUsername("fakeUser");
//    }
//
//    @Test
//    public void testAuthenticateThrowsBadCredentialsForBadPassword() {
//        UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken("activeUser", "badPassword");
//
//        assertThatThrownBy(() -> ajaxLoginAuthenticationProvider.authenticate(authenticationToken))
//                .isInstanceOf(BadCredentialsException.class)
//                .hasMessage("Bad credentials for username: activeUser");
//
//        verify(userService, Mockito.times(1)).findByUsername("activeUser");
//    }
//
//    @Test
//    public void testSupportValidAuthenticationClass() {
//        assertThat(ajaxLoginAuthenticationProvider.supports(UsernamePasswordAuthenticationToken.class))
//                .isTrue();
//    }
//
//    @Test
//    public void testSupportInvalidAuthenticationClass() {
//        assertThat(ajaxLoginAuthenticationProvider.supports(JwtAuthenticationToken.class))
//                .isFalse();
//    }
}
