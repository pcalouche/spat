package com.pcalouche.spat.interceptors;

import com.pcalouche.spat.dao.authorization.AuthorizationDao;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@Component
public class AuthorizationInterceptor extends HandlerInterceptorAdapter {
    private final Logger logger = LoggerFactory.getLogger(getClass());
    private final AuthorizationDao authorizationDao;

    @Autowired
    public AuthorizationInterceptor(AuthorizationDao authorizationDao) {
        this.authorizationDao = authorizationDao;
    }

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        logger.info("in pre handle " + request.getMethod() + " " + request.getRequestURL() + " " + request.getHeader("Accept"));
        if (request.getHeader("Accept").toLowerCase().contains("application/json") && !authorizationDao.isAuthorized(request.getHeader("AUTH_TOKEN"))) {
            throw new SecurityException("AUTH_TOKEN is invalid");
        }
        return true;
    }
}
