package com.pcalouche.spat.exception;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.api.dto.TeamEditRequest;
import com.pcalouche.spat.security.util.SecurityUtils;
import io.jsonwebtoken.JwtException;
import org.junit.jupiter.api.Test;
import org.springframework.core.MethodParameter;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConversionException;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockServletContext;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import java.io.IOException;

import static org.assertj.core.api.Assertions.assertThat;

public class ApiErrorResponseTest {
    private final MockHttpServletRequest request = MockMvcRequestBuilders.post(Endpoints.USERS)
            .queryParam("dummyParam", "dummyValue")
            .header(HttpHeaders.AUTHORIZATION, SecurityUtils.AUTH_HEADER_BEARER_PREFIX + "goodToken")
            .contentType(MediaType.APPLICATION_JSON)
            .buildRequest(new MockServletContext());

    @Test
    public void testApiErrorResponse() {
        ApiErrorResponse apiErrorResponse = new ApiErrorResponse(new ApiNotFoundException(), request);

        assertThat(apiErrorResponse.getTimestamp()).isNotNull();
        assertThat(apiErrorResponse.getPath()).isEqualTo("/users?dummyParam=dummyValue");
        assertThat(apiErrorResponse.getMessage()).isEqualTo(ApiNotFoundException.DEFAULT_MESSAGE);
        assertThat(apiErrorResponse.getException()).isEqualTo(ApiNotFoundException.class.getName());
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.NOT_FOUND);
        assertThat(apiErrorResponse.getStatus()).isEqualTo(HttpStatus.NOT_FOUND.value());
        assertThat(apiErrorResponse.getReasonPhrase()).isEqualTo(HttpStatus.NOT_FOUND.getReasonPhrase());
        assertThat(apiErrorResponse.getValidationMessages()).isNull();
    }

    @Test
    public void testApiErrorResponseHttpStatus() throws NoSuchMethodException {
        // AuthenticationException case
        ApiErrorResponse apiErrorResponse = new ApiErrorResponse(new BadCredentialsException("blah"), request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.UNAUTHORIZED);

        // JwtException case
        apiErrorResponse = new ApiErrorResponse(new JwtException("blah"), request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.UNAUTHORIZED);

        // AccessDeniedException case
        apiErrorResponse = new ApiErrorResponse(new AccessDeniedException("blah"), request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.FORBIDDEN);

        // HttpMessageConversionException case
        apiErrorResponse = new ApiErrorResponse(new HttpMessageConversionException("blah"), request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY);

        TeamEditRequest teamEditRequest = new TeamEditRequest();
        BeanPropertyBindingResult errors = new BeanPropertyBindingResult(teamEditRequest, "teamEditRequest");
        errors.rejectValue(
                "name",
                "name",
                "Required"
        );

        //MethodArgumentNotValidException case
        MethodArgumentNotValidException methodArgumentNotValidException = new MethodArgumentNotValidException(
                new MethodParameter(getClass().getDeclaredMethod("testApiErrorResponseHttpStatus"), -1),
                errors
        );

        apiErrorResponse = new ApiErrorResponse(methodArgumentNotValidException, request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY);

        // MethodArgumentTypeMismatchException case
        MethodArgumentTypeMismatchException methodArgumentTypeMismatchException = new MethodArgumentTypeMismatchException(
                null,
                null,
                "name",
                new MethodParameter(getClass().getDeclaredMethod("testApiErrorResponseHttpStatus"), -1),
                new RuntimeException("bad argument"));

        apiErrorResponse = new ApiErrorResponse(methodArgumentTypeMismatchException, request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY);

        // BindException case
        BindException bindException = new BindException(errors, "");

        apiErrorResponse = new ApiErrorResponse(bindException, request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY);

        // ApiException case
        apiErrorResponse = new ApiErrorResponse(new ApiException("blah", HttpStatus.CONFLICT), request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.CONFLICT);

        // All other exceptions case
        apiErrorResponse = new ApiErrorResponse(new RuntimeException("blah"), request);
        assertThat(apiErrorResponse.getHttpStatus()).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @Test
    public void testApiErrorResponsePopulatesValidationMessages() throws NoSuchMethodException {
        TeamEditRequest teamEditRequest = new TeamEditRequest();
        BeanPropertyBindingResult errors = new BeanPropertyBindingResult(teamEditRequest, "teamEditRequest");
        errors.rejectValue(
                "name",
                "name",
                "Required"
        );

        MethodArgumentNotValidException methodArgumentNotValidException = new MethodArgumentNotValidException(
                new MethodParameter(getClass().getDeclaredMethod("testApiErrorResponsePopulatesValidationMessages"), -1),
                errors
        );
        ApiErrorResponse apiErrorResponse = new ApiErrorResponse(methodArgumentNotValidException, request);
        assertThat(apiErrorResponse.getValidationMessages().get("name")).isEqualTo("Required");

        BindException bindException = new BindException(errors);
        apiErrorResponse = new ApiErrorResponse(bindException, request);
        assertThat(apiErrorResponse.getValidationMessages().get("name")).isEqualTo("Required");
    }

    @Test
    public void testWriteExceptionToResponse() throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        MockHttpServletResponse response = new MockHttpServletResponse();
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .buildRequest(new MockServletContext());

        AuthenticationException authenticationException = new BadCredentialsException("bad credentials");

        ApiErrorResponse.writeExceptionToResponse(authenticationException, request, response);

        JsonNode jsonNode = objectMapper.readValue(response.getContentAsString(), JsonNode.class);
        assertThat(jsonNode.get("timestamp").textValue()).isNotNull();
        assertThat(jsonNode.get("path").textValue()).isEqualTo(request.getRequestURI());
        assertThat(jsonNode.get("message").textValue()).isEqualTo(authenticationException.getMessage());
        assertThat(jsonNode.get("exception").textValue()).isEqualTo(BadCredentialsException.class.getName());
        assertThat(jsonNode.get("status").intValue()).isEqualTo(HttpStatus.UNAUTHORIZED.value());
        assertThat(jsonNode.get("reasonPhrase").textValue()).isEqualTo(HttpStatus.UNAUTHORIZED.getReasonPhrase());
        assertThat(jsonNode.get("validationMessages")).isNull();
    }
}
