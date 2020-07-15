package com.pcalouche.spat.exception;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.jsonwebtoken.JwtException;
import lombok.EqualsAndHashCode;
import lombok.Getter;
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
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@EqualsAndHashCode
public class ApiErrorResponse {
    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final String VALIDATION_MESSAGES = "Validation failed. See validation messages.";
    private final String timestamp;
    private final String path;
    private final String message;
    private final String exception;
    @JsonIgnore
    private final HttpStatus httpStatus;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private final Map<String, String> validationMessages;

    public ApiErrorResponse(Exception e, HttpServletRequest request) {
        timestamp = ZonedDateTime.now().toOffsetDateTime().toString();
        path = StringUtils.isNotBlank(request.getQueryString()) ?
                request.getRequestURI() + "?" + request.getQueryString()
                :
                request.getRequestURI();
        exception = e.getClass().getName();

        if (e instanceof AuthenticationException ||
                e instanceof JwtException) {
            httpStatus = HttpStatus.UNAUTHORIZED;
        } else if (e instanceof AccessDeniedException) {
            httpStatus = HttpStatus.FORBIDDEN;
        } else if (e instanceof HttpMessageConversionException ||
                e instanceof MethodArgumentNotValidException ||
                e instanceof MethodArgumentTypeMismatchException ||
                e instanceof BindException) {
            httpStatus = HttpStatus.UNPROCESSABLE_ENTITY;
        } else if (e instanceof ApiException) {
            httpStatus = ((ApiException) e).getStatus();
        } else {
            httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        }

        if (e instanceof MethodArgumentNotValidException || e instanceof BindException) {
            message = VALIDATION_MESSAGES;
            validationMessages = getValidationMessages(e);
        } else {
            message = e.getMessage();
            validationMessages = null;
        }
    }

    public static void writeExceptionToResponse(Exception e, HttpServletRequest request, HttpServletResponse response)
            throws IOException {
        ApiErrorResponse apiErrorResponse = new ApiErrorResponse(e, request);
        response.setStatus(apiErrorResponse.getStatus());
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        objectMapper.writeValue(response.getWriter(), apiErrorResponse);
    }

    private static Map<String, String> getValidationMessages(Exception e) {
        Map<String, String> validateMessages = new HashMap<>();
        List<FieldError> fieldErrors;
        if (e instanceof MethodArgumentNotValidException) {
            MethodArgumentNotValidException methodArgumentNotValidException = (MethodArgumentNotValidException) e;
            fieldErrors = methodArgumentNotValidException.getBindingResult().getFieldErrors();
        } else {
            BindException bindException = (BindException) e;
            fieldErrors = bindException.getBindingResult().getFieldErrors();
        }
        fieldErrors.forEach(fieldError -> validateMessages.put(fieldError.getField(), fieldError.getDefaultMessage()));
        return validateMessages;
    }

    public int getStatus() {
        return httpStatus.value();
    }

    public String getReasonPhrase() {
        return httpStatus.getReasonPhrase();
    }
}
