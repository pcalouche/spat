package com.pcalouche.spat.exception;

import org.springframework.http.HttpStatus;

public class ApiForbiddenException extends ApiException {
    public static final String DEFAULT_MESSAGE = "Resource access, creation, or modification is forbidden.";

    public ApiForbiddenException() {
        this(DEFAULT_MESSAGE);
    }

    public ApiForbiddenException(String message) {
        super(message, HttpStatus.FORBIDDEN);
    }
}
