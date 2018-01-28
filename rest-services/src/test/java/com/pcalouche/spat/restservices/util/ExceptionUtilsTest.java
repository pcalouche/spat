package com.pcalouche.spat.restservices.util;

import com.pcalouche.spat.restservices.AbstractUnitTest;
import io.jsonwebtoken.ExpiredJwtException;
import org.junit.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.converter.HttpMessageConversionException;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.AuthenticationException;

import java.io.IOException;

import static org.assertj.core.api.Assertions.assertThat;

public class ExceptionUtilsTest extends AbstractUnitTest {
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
        assertThat(httpStatus).isEqualTo(HttpStatus.BAD_REQUEST);
    }

    @Test
    public void testHttpStatusForMethodArgumentNotValidException() {

    }

    @Test
    public void testHttpStatusForAllOtherExceptions() {
        HttpStatus httpStatus = ExceptionUtils.getHttpStatusForException(new RuntimeException("message"));
        assertThat(httpStatus).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @Test
    public void testWriteExceptionToResponse() throws IOException {
        MockHttpServletResponse response = new MockHttpServletResponse();
        AuthenticationException authenticationException = new BadCredentialsException("bad credentials");

        ExceptionUtils.writeExceptionToResponse(authenticationException, response);

        assertThat(response.getContentAsString()).isEqualTo(ExceptionUtils.buildJsonErrorObject(authenticationException).toString());
    }
}
