package com.pcalouche.spat.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.pcalouche.spat.util.ExceptionUtils;
import com.pcalouche.spat.util.LoggerUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice(basePackages = "com.pcalouche.spat")
public class ControllerExceptionAdvice {
    @ExceptionHandler({Exception.class})
    public ResponseEntity<JsonNode> exceptionResponse(Exception e) throws Exception {
        LoggerUtils.logException(e);
        return new ResponseEntity<>(ExceptionUtils.buildJsonErrorObject(e), ExceptionUtils.getHttpStatusForException(e));
    }
}
