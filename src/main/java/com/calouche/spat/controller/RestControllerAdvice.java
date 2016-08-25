package com.calouche.spat.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestController;

@ControllerAdvice(annotations = RestController.class)
public class RestControllerAdvice {
    private static final Logger logger = LoggerFactory.getLogger(RestController.class);
//
//    @ExceptionHandler(Exception.class)
//    public void exception(Exception e) throws Exception {
//        logger.error("Exception Occurred", e);
//        throw e;
//    }

    @ExceptionHandler(Exception.class)
    public String exception(Exception e) throws Exception {
        logger.error("Exception Occurred", e);
        return "Unable to talk to database.  Please try again later.";
    }
}
