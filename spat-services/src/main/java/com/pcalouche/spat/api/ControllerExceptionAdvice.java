package com.pcalouche.spat.api;

import com.pcalouche.spat.exception.ExceptionUtils;
import com.pcalouche.spat.exception.JsonExceptionResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import javax.servlet.http.HttpServletRequest;

@ControllerAdvice(basePackages = "com.pcalouche.spat")
public class ControllerExceptionAdvice {
    private static final Logger logger = LoggerFactory.getLogger(ControllerExceptionAdvice.class);

    @ExceptionHandler({Exception.class})
    public ResponseEntity<JsonExceptionResponse> exceptionResponse(Exception e, HttpServletRequest request) {
        logger.error(String.format("%s occurred", e.getClass().getName()), e);
        return new ResponseEntity<>(ExceptionUtils.buildJsonErrorResponse(e, request), ExceptionUtils.getHttpStatusForException(e));
    }
}
