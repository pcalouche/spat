package com.pcalouche.spat.api.auth.controller;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = AuthUris.ROOT)
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
    //    public AuthResponse token(@Valid @RequestBody AuthRequest authRequest, HttpServletRequest request) {
    //        Authentication authenticationFromClient = new UsernamePasswordAuthenticationToken(
    //                authRequest.getUsername(),
    //                authRequest.getPassword()
    //        );
    //
    //        Authentication authenticationFromServer = authenticationManager.authenticate(authenticationFromClient);
    //        return SecurityUtils.createAuthResponse(authenticationFromServer);
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
