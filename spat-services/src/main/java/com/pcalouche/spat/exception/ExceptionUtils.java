package com.pcalouche.spat.exception;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.api.exception.RestResourceForbiddenException;
import com.pcalouche.spat.api.exception.RestResourceNotFoundException;
import io.jsonwebtoken.JwtException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConversionException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ExceptionUtils {
    private static final ObjectMapper objectMapper = new ObjectMapper();

    public static JsonExceptionResponse buildJsonErrorResponse(Exception e, HttpServletRequest request) {
        JsonExceptionResponse jsonExceptionResponse = new JsonExceptionResponse();
        jsonExceptionResponse.setTimestamp(System.currentTimeMillis());
        if (request != null) {
            jsonExceptionResponse.setPath(StringUtils.isNotBlank(request.getQueryString()) ?
                    request.getRequestURI() + "?" + request.getQueryString()
                    :
                    request.getRequestURI()
            );
        }
        HttpStatus httpStatus = getHttpStatusForException(e);
        jsonExceptionResponse.setStatus(httpStatus.value());
        jsonExceptionResponse.setError(httpStatus.getReasonPhrase());
        jsonExceptionResponse.setException(e.getClass().getSimpleName());
        if (e instanceof MethodArgumentNotValidException) {
            jsonExceptionResponse.setMessage("See validation messages for more details.");
            jsonExceptionResponse.setValidationMessages(getFieldErrorsMap(e));
        } else {
            jsonExceptionResponse.setMessage(e.getMessage());
        }
        return jsonExceptionResponse;
    }

    public static HttpStatus getHttpStatusForException(Exception e) {
        if (e instanceof AuthenticationException || e instanceof JwtException) {
            return HttpStatus.UNAUTHORIZED;
        } else if (e instanceof AccessDeniedException ||
                e instanceof RestResourceForbiddenException) {
            return HttpStatus.FORBIDDEN;
        } else if (e instanceof HttpMessageConversionException ||
                e instanceof MethodArgumentNotValidException ||
                e instanceof MethodArgumentTypeMismatchException) {
            return HttpStatus.UNPROCESSABLE_ENTITY;
        } else if (e instanceof RestResourceNotFoundException) {
            return HttpStatus.NOT_FOUND;
        } else {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }

    public static void writeExceptionToResponse(Exception e, HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setStatus(getHttpStatusForException(e).value());
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        objectMapper.writeValue(response.getWriter(), ExceptionUtils.buildJsonErrorResponse(e, request));
    }

    private static Map<String, String> getFieldErrorsMap(Exception e) {
        Map<String, String> errorMap = new HashMap<>();
        List<FieldError> fieldErrors;
        if (e instanceof MethodArgumentNotValidException) {
            MethodArgumentNotValidException methodArgumentNotValidException = (MethodArgumentNotValidException) e;
            fieldErrors = methodArgumentNotValidException.getBindingResult().getFieldErrors();
        } else {
            BindException bindException = (BindException) e;
            fieldErrors = bindException.getBindingResult().getFieldErrors();
        }
        fieldErrors.forEach(fieldError -> errorMap.put(fieldError.getField(), fieldError.getDefaultMessage()));
        return errorMap;
    }
}