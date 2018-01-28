package com.pcalouche.spat.restservices.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.pcalouche.spat.restservices.util.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice(basePackages = "com.pcalouche.spat")
public class ControllerExceptionAdvice {
    private static final Logger logger = LoggerFactory.getLogger(ControllerExceptionAdvice.class);

    @ExceptionHandler({Exception.class})
    public ResponseEntity<JsonNode> exceptionResponse(Exception e) {
        logger.error(String.format("%s occurred", e.getClass().getName()), e);
        return new ResponseEntity<>(ExceptionUtils.buildJsonErrorObject(e), ExceptionUtils.getHttpStatusForException(e));
    }
}
