package com.pcalouche.spat.controller.auth;

import com.pcalouche.spat.controller.AbstractController;
import com.pcalouche.spat.security.LoginRequest;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletResponse;

@RestController
@RequestMapping(value = AuthUris.ROOT)
public class AuthorizationController extends AbstractController {

    @PostMapping(value = AuthUris.TOKEN)
    public void token(@RequestBody LoginRequest loginRequest, HttpServletResponse response) {
        logger.info("in token");
        logger.info(loginRequest.getUsername() + " " + loginRequest.getPassword());
    }

    @PostMapping(value = AuthUris.TOKEN + "2")
    public void token2(@RequestBody LoginRequest loginRequest, HttpServletResponse response) {
        logger.info("in token");
        logger.info(loginRequest.getUsername() + " " + loginRequest.getPassword());
    }
}
