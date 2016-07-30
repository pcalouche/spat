package com.calouche.spat.controller;

import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestController;

@ControllerAdvice(annotations = RestController.class)
public class RestControllerAdvice {
    @ExceptionHandler(RestControllerException.class)
    public String exception(RestControllerException e) {
        e.printStackTrace();
        return e.getMessage();
    }
}
