package com.pcalouche.spat.exception;

import org.springframework.http.HttpStatus;

public class ApiNotFoundException extends ApiException {
    public static final String DEFAULT_MESSAGE = "Resource not found.";

    public ApiNotFoundException() {
        this(DEFAULT_MESSAGE);
    }

    public ApiNotFoundException(String message) {
        super(message, HttpStatus.NOT_FOUND);
    }
}
