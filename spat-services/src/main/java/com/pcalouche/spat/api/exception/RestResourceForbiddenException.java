package com.pcalouche.spat.api.exception;

/**
 * Exception to throw when a REST resource cannot be accessed or
 * deleted.
 */
public class RestResourceForbiddenException extends RuntimeException {
    public RestResourceForbiddenException(String message) {
        super(message);
    }

    public RestResourceForbiddenException(String message, Throwable t) {
        super(message, t);
    }
}
