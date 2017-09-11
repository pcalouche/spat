package com.pcalouche.spat.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice(basePackages = "com.pcalouche.spat")
public class ControllerExceptionAdvice {
    private final Logger logger = LoggerFactory.getLogger(this.getClass());
    private static final ObjectMapper objectMapper = new ObjectMapper();

    @ExceptionHandler(SecurityException.class)
    public ResponseEntity<String> securityException(SecurityException e) throws Exception {
        logger.error("Uncaught SecurityException", e);
        return new ResponseEntity<>(buildErrorObject(e).toString(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler({DataAccessException.class})
    public ResponseEntity<String> dataAccessException(Exception e) throws Exception {
        logger.error("Uncaught DataAccessException", e);
        return new ResponseEntity<>(buildErrorObject(e).toString(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> exception(Exception e) throws Exception {
        logger.error("Uncaught Exception", e);
        return new ResponseEntity<>(buildErrorObject(e).toString(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    public static JsonNode buildErrorObject(Exception e) {
        ObjectNode errorObjectNode = objectMapper.createObjectNode();
        errorObjectNode.put("type", e.getClass().getName());
        errorObjectNode.put("message", e.getMessage());
        return objectMapper.createObjectNode().set("error", errorObjectNode);
    }
}
