package com.calouche.spat.interceptors;

import com.calouche.spat.dao.authorization.AuthorizationDao;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@Component
public class AuthorizationInterceptor extends HandlerInterceptorAdapter {
    private static final Logger logger = LoggerFactory.getLogger(AuthorizationInterceptor.class);

    @Autowired
    private AuthorizationDao authorizationDao;


    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        logger.info("in pre handle " + request.getMethod() + " " + request.getRequestURL());
        if (request.getHeader("Accept").contains("text/html")) {
            return true;
        } else {
            return authorizationDao.isAuthorized(request.getHeader("AUTH_TOKEN"));
        }
    }
}