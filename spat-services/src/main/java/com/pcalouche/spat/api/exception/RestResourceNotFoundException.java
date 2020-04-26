package com.pcalouche.spat.api.exception;

/**
 * Exception to throw when a REST resource is not found.
 */
public class RestResourceNotFoundException extends RuntimeException {
    public RestResourceNotFoundException(String message) {
        super(message);
    }

    public RestResourceNotFoundException(String message, Throwable t) {
        super(message, t);
    }
}
