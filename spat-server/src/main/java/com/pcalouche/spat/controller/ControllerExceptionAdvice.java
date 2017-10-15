package com.pcalouche.spat.controller;

import com.pcalouche.spat.util.ExceptionUtils;
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

    @ExceptionHandler({DataAccessException.class})
    public ResponseEntity<String> dataAccessException(Exception e) throws Exception {
        logger.error("Uncaught DataAccessException", e);
        return new ResponseEntity<>(ExceptionUtils.buildJsonErrorObject(e).toString(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> exception(Exception e) throws Exception {
        logger.error("Uncaught Exception", e);
        return new ResponseEntity<>(ExceptionUtils.buildJsonErrorObject(e).toString(), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
