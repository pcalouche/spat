package com.calouche.spat.controller;

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
import org.springframework.web.bind.annotation.RestController;

@ControllerAdvice(annotations = RestController.class)
public class RestControllerAdvice {
    private static final Logger logger = LoggerFactory.getLogger(RestController.class);
    private static final ObjectMapper objectMapper = new ObjectMapper();

    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> exception(Exception e) throws Exception {
        logger.error("Exception Exception Occurred", e);
        return new ResponseEntity<>(buildErrorObject(e).toString(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(DataAccessException.class)
    public ResponseEntity<String> dataAccessException(DataAccessException e) throws Exception {
        logger.error("DataAccessException Exception Occurred", e);
        return new ResponseEntity<>(buildErrorObject(e).toString(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    private JsonNode buildErrorObject(Exception e) {
        ObjectNode errorObjectNode = objectMapper.createObjectNode();
        errorObjectNode.put("type", e.getClass().getName());
        errorObjectNode.put("message", e.getMessage());
        return objectMapper.createObjectNode().set("error", errorObjectNode);
    }
}
