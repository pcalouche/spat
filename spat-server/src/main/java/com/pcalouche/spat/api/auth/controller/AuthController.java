package com.pcalouche.spat.api.auth.controller;

//@RestController
//@RequestMapping(value = AuthUris.ROOT)
public class AuthController {
    //    private final AuthenticationManager authenticationManager;
    //    @Autowired
    //    UserDao userDao;
    //
    //    @Autowired
    //    public AuthController(AuthenticationManager authenticationManager) {
    //        this.authenticationManager = authenticationManager;
    //    }
    //
    //    @PostMapping(value = AuthUris.TOKEN)
    //    public AuthResponse token() {
    //        return SecurityUtils.createAuthResponse(SecurityContextHolder.getContext().getAuthentication());
    //    }
    //
    //    @GetMapping(value = AuthUris.REFRESH_TOKEN)
    //    public AuthResponse refreshToken(@RequestHeader(value = HttpHeaders.AUTHORIZATION) String authHeader, HttpServletRequest request) {
    //        // Process the JWT that came from the client and mark it as a refresh token.  The authentication provider does some additional
    //        // logic on refresh tokens.
    //        JwtAuthenticationToken authenticationFromClient = new JwtAuthenticationToken(SecurityUtils.getTokenFromHeaderValue(authHeader));
    //        authenticationFromClient.setDetails("refreshToken");
    //        Authentication authenticationFromServer = authenticationManager.authenticate(authenticationFromClient);
    //        return SecurityUtils.createAuthResponse(authenticationFromServer);
    //    }
}
