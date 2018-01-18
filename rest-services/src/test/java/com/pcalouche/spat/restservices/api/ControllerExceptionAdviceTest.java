package com.pcalouche.spat.restservices.api;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.api.user.controller.UserEndpoints;
import com.pcalouche.spat.restservices.util.ExceptionUtils;
import org.junit.Before;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = UserController.class)
public class ControllerExceptionAdviceTest extends AbstractControllerTest {
    @MockBean
    private UserController userController;
    private MockHttpServletRequestBuilder request;

    @Before
    public void beforeLocal() throws JsonProcessingException {
        request = MockMvcRequestBuilders.get(UserEndpoints.ROOT);
    }

    //    @Test
    //    public void testBadCredentialsException() throws Exception {
    //        BadCredentialsException badCredentialsException = new BadCredentialsException("bad credentials");
    //        JsonNode jsonNode = ExceptionUtils.buildJsonErrorObject(badCredentialsException);
    //        given(userController.getUsers()).willThrow(badCredentialsException);
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.UNAUTHORIZED.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(BadCredentialsException.class);
    //    }
    //
    //    @Test
    //    public void testAccountStatusException() throws Exception {
    //        AccountStatusException accountStatusException = new AccountExpiredException("account expired");
    //        JsonNode jsonNode = ExceptionUtils.buildJsonErrorObject(accountStatusException);
    //        given(userController.getUsers()).willThrow(accountStatusException);
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.UNAUTHORIZED.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(AccountStatusException.class);
    //    }
    //
    //    @Test
    //    public void testJwtException() throws Exception {
    //        JwtException jwtException = new JwtException("bad JWT");
    //        JsonNode jsonNode = ExceptionUtils.buildJsonErrorObject(jwtException);
    //        given(userController.getUsers()).willThrow(jwtException);
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.UNAUTHORIZED.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(JwtException.class);
    //    }
    //
    //    @Test
    //    public void testAccessDeniedException() throws Exception {
    //        AccessDeniedException accessDeniedException = new AccessDeniedException("user does not have access to this endpoint");
    //        JsonNode jsonNode = ExceptionUtils.buildJsonErrorObject(accessDeniedException);
    //        given(userController.getUsers()).willThrow(accessDeniedException);
    //
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.FORBIDDEN.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(AccessDeniedException.class);
    //    }

    @Test
    public void testDataAccessException() throws Exception {
        DataAccessException exception = new EmptyResultDataAccessException(1);
        JsonNode jsonNode = ExceptionUtils.buildJsonErrorObject(exception);
        given(userController.getUsers()).willThrow(exception);

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
                .andExpect(content().json(jsonNode.toString()))
                .andReturn();

        assertThat(mvcResult.getResolvedException()).isInstanceOf(DataAccessException.class);
    }

    @Test
    public void testException() throws Exception {
        RuntimeException runtimeException = new RuntimeException("some random runtime exception");
        JsonNode jsonNode = ExceptionUtils.buildJsonErrorObject(runtimeException);
        given(userController.getUsers()).willThrow(runtimeException);

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
                .andExpect(content().json(jsonNode.toString()))
                .andReturn();

        assertThat(mvcResult.getResolvedException()).isInstanceOf(Exception.class);
    }

    //    // Mock the controller up, so we can return some dummy exceptions
    //    @MockBean
    //    private PingController pingController;
    //    private MockHttpServletRequestBuilder request;
    //
    //    @Before
    //    public void beforeLocal() throws JsonProcessingException {
    //        request = get(PingUris.ROOT)
    //                .contentType(MediaType.APPLICATION_JSON);
    //    }
    //
    //    @Test
    //    public void testAuthenticationException() throws Exception {
    //        // Testing one example of AuthenticationException should cover them all
    //        AuthenticationException authenticationException = new BadCredentialsException("bad credentials");
    //        JsonNode jsonNode = ControllerExceptionAdvice.buildJsonErrorObject(authenticationException);
    //        given(pingController.ping()).willThrow(authenticationException);
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.UNAUTHORIZED.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(BadCredentialsException.class);
    //    }
    //
    //    @Test
    //    public void testJwtException() throws Exception {
    //        JwtException jwtException = new JwtException("bad JWT");
    //        JsonNode jsonNode = ControllerExceptionAdvice.buildJsonErrorObject(jwtException);
    //        given(pingController.ping()).willThrow(jwtException);
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.UNAUTHORIZED.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(JwtException.class);
    //    }
    //
    //    @Test
    //    public void testAccessDeniedException() throws Exception {
    //        AccessDeniedException accessDeniedException = new AccessDeniedException("user does not have access to this endpoint");
    //        JsonNode jsonNode = ControllerExceptionAdvice.buildJsonErrorObject(accessDeniedException);
    //        given(pingController.ping()).willThrow(accessDeniedException);
    //
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.FORBIDDEN.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(AccessDeniedException.class);
    //    }
    //
    //    @Test
    //    public void testHttpMessageConversionException() throws Exception {
    //        HttpMessageConversionException httpMessageConversionException = new HttpMessageConversionException("http conversion exception");
    //        JsonNode jsonNode = ControllerExceptionAdvice.buildJsonErrorObject(httpMessageConversionException);
    //        given(pingController.ping()).willThrow(httpMessageConversionException);
    //
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.BAD_REQUEST.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(HttpMessageConversionException.class);
    //    }
    //
    //    @Test
    //    public void testMethodArgumentNotValidException() throws Exception {
    //        // Use a request that will actually generate a MethodArgumentNotValidException because instantiating
    //        // one of these is a pain.
    //        AuthRequest authRequest = new AuthRequest();
    //        authRequest.setUsername("user");
    //        authRequest.setPassword(null);
    //
    //        request = post(String.format("%s/%s", AuthUris.ROOT, AuthUris.TOKEN))
    //                .contentType(MediaType.APPLICATION_JSON)
    //                .content(objectMapper.writeValueAsString(authRequest));
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.BAD_REQUEST.value()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(MethodArgumentNotValidException.class);
    //    }
    //
    //    @Test
    //    public void testSpringDataAccessException() throws Exception {
    //        org.springframework.dao.DataAccessException exception = new EmptyResultDataAccessException(1);
    //        JsonNode jsonNode = ControllerExceptionAdvice.buildJsonErrorObject(exception);
    //        given(pingController.ping()).willThrow(exception);
    //
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(org.springframework.dao.DataAccessException.class);
    //    }
    //
    //    @Test
    //    public void testJooqDataAccessException() throws Exception {
    //        org.jooq.exception.DataAccessException exception = new org.jooq.exception.DataAccessException("jOOQ exception");
    //        JsonNode jsonNode = ControllerExceptionAdvice.buildJsonErrorObject(exception);
    //        given(pingController.ping()).willThrow(exception);
    //
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(org.jooq.exception.DataAccessException.class);
    //    }
    //
    //    @Test
    //    public void testException() throws Exception {
    //        RuntimeException runtimeException = new RuntimeException("some random runtime exception");
    //        JsonNode jsonNode = ControllerExceptionAdvice.buildJsonErrorObject(runtimeException);
    //        given(pingController.ping()).willThrow(runtimeException);
    //
    //        MvcResult mvcResult = mockMvc.perform(request)
    //                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
    //                .andExpect(content().json(jsonNode.toString()))
    //                .andReturn();
    //
    //        assertThat(mvcResult.getResolvedException()).isInstanceOf(Exception.class);
    //    }
}
