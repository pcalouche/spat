package com.pcalouche.spat.exception;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.api.dto.TeamDto;
import com.pcalouche.spat.api.dto.TeamEditRequest;
import com.pcalouche.spat.api.exception.RestResourceForbiddenException;
import com.pcalouche.spat.api.exception.RestResourceNotFoundException;
import io.jsonwebtoken.ExpiredJwtException;
import org.junit.jupiter.api.Test;
import org.springframework.core.MethodParameter;
import org.springframework.http.HttpStatus;
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

public class ExceptionUtilsTest {

    @Test
    public void testBuildJsonErrorResponse() throws NoSuchMethodException {
        long currentTimeMillis = System.currentTimeMillis();
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .buildRequest(new MockServletContext());

        TeamEditRequest teamEditRequest = new TeamEditRequest();
        BeanPropertyBindingResult errors = new BeanPropertyBindingResult(teamEditRequest, "teamEditRequest");
        errors.rejectValue(
                "name",
                "name",
                "Required"
        );

        MethodArgumentNotValidException methodArgumentNotValidException = new MethodArgumentNotValidException(
                new MethodParameter(getClass().getDeclaredMethod("testBuildJsonErrorResponse"), -1),
                errors
        );

        // Test response that will include validation messages
        JsonExceptionResponse jsonExceptionResponse = ExceptionUtils.buildJsonErrorResponse(methodArgumentNotValidException, request);
        assertThat(jsonExceptionResponse.getTimestamp()).isGreaterThanOrEqualTo(currentTimeMillis);
        assertThat(jsonExceptionResponse.getStatus()).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY.value());
        assertThat(jsonExceptionResponse.getError()).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY.getReasonPhrase());
        assertThat(jsonExceptionResponse.getException()).isEqualTo(MethodArgumentNotValidException.class.getSimpleName());
        assertThat(jsonExceptionResponse.getMessage()).isEqualTo("See validation messages for more details.");
        assertThat(jsonExceptionResponse.getValidationMessages()).isNotNull();
        assertThat(jsonExceptionResponse.getValidationMessages().get("name")).isEqualTo("Required");

        // Test response that will not include validation messages
        jsonExceptionResponse = ExceptionUtils.buildJsonErrorResponse(new RestResourceForbiddenException("can touch this"), request);
        assertThat(jsonExceptionResponse.getTimestamp()).isGreaterThanOrEqualTo(currentTimeMillis);
        assertThat(jsonExceptionResponse.getStatus()).isEqualTo(HttpStatus.FORBIDDEN.value());
        assertThat(jsonExceptionResponse.getError()).isEqualTo(HttpStatus.FORBIDDEN.getReasonPhrase());
        assertThat(jsonExceptionResponse.getException()).isEqualTo(RestResourceForbiddenException.class.getSimpleName());
        assertThat(jsonExceptionResponse.getValidationMessages()).isNull();
    }

    @Test
    public void testHttpStatusForAuthenticationException() {
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(new BadCredentialsException("message"));
        assertThat(httpStatus).isEqualTo(HttpStatus.UNAUTHORIZED);
    }

    @Test
    public void testHttpStatusForJwtException() {
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(new ExpiredJwtException(null, null, "message"));
        assertThat(httpStatus).isEqualTo(HttpStatus.UNAUTHORIZED);
    }

    @Test
    public void testHttpStatusForAccessDeniedException() {
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(new AccessDeniedException("message"));
        assertThat(httpStatus).isEqualTo(HttpStatus.FORBIDDEN);
    }

    @Test
    public void testHttpStatusForMethodHttpMessageConversionException() {
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(new HttpMessageConversionException("message"));
        assertThat(httpStatus).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY);
    }

    @Test
    public void testHttpStatusForMethodArgumentNotValidException() throws NoSuchMethodException {
        MethodArgumentNotValidException methodArgumentNotValidException = new MethodArgumentNotValidException(
                new MethodParameter(getClass().getDeclaredMethod("testHttpStatusForMethodArgumentNotValidException"), -1),
                new BindException(new TeamDto(), "bad team argument")
        );
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(methodArgumentNotValidException);
        assertThat(httpStatus).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY);
    }

    @Test
    public void testHttpStatusForMethodMethodArgumentTypeMismatchException() throws NoSuchMethodException {
        MethodArgumentTypeMismatchException methodArgumentTypeMismatchException = new MethodArgumentTypeMismatchException(
                null,
                null,
                "name",
                new MethodParameter(getClass().getDeclaredMethod("testHttpStatusForMethodArgumentNotValidException"), -1),
                new RuntimeException("bad argument"));
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(methodArgumentTypeMismatchException);
        assertThat(httpStatus).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY);
    }

    @Test
    public void testHttpStatusForRestResourceNotFoundException() {
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(new RestResourceNotFoundException("message"));
        assertThat(httpStatus).isEqualTo(HttpStatus.NOT_FOUND);
    }

    @Test
    public void testHttpStatusForRestResourceForbiddenException() {
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(new RestResourceForbiddenException("message"));
        assertThat(httpStatus).isEqualTo(HttpStatus.FORBIDDEN);
    }

    @Test
    public void testHttpStatusForAllOtherExceptions() {
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(new RuntimeException("message"));
        assertThat(httpStatus).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @Test
    public void testWriteExceptionToResponse() throws IOException {
        long currentTimeInMillis = System.currentTimeMillis();
        ObjectMapper objectMapper = new ObjectMapper();
        MockHttpServletResponse response = new MockHttpServletResponse();
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .buildRequest(new MockServletContext());

        AuthenticationException authenticationException = new BadCredentialsException("bad credentials");

        JsonExceptionResponse expectedJsonExceptionResponse = ExceptionUtils.buildJsonErrorResponse(authenticationException, request);
        // Remove timestamp for easier comparison
        expectedJsonExceptionResponse.setTimestamp(0);

        ExceptionUtils.writeExceptionToResponse(authenticationException, request, response);

        JsonExceptionResponse actualJsonExceptionResponse = objectMapper.readValue(response.getContentAsString(), JsonExceptionResponse.class);

        assertThat(actualJsonExceptionResponse.getTimestamp() >= currentTimeInMillis);
        // Set timestamp to 0 for reach comparision
        actualJsonExceptionResponse.setTimestamp(0);

        assertThat(actualJsonExceptionResponse).isEqualTo(expectedJsonExceptionResponse);
    }
}
