package com.pcalouche.spat.api;

import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.api.controller.UserController;
import com.pcalouche.spat.exception.ExceptionUtils;
import com.pcalouche.spat.exception.JsonExceptionResponse;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockServletContext;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = UserController.class)
public class ControllerExceptionAdviceTest extends AbstractControllerTest {
    @MockBean
    private UserController userController;

    @Test
    public void testException() throws Exception {
        long currentTimeInMillis = System.currentTimeMillis();
        RuntimeException runtimeException = new RuntimeException("some random runtime exception");
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        request.setRequestURI(Endpoints.USERS);

        JsonExceptionResponse expectedJsonExceptionResponse = ExceptionUtils.buildJsonErrorResponse(runtimeException, request);
        // Remove timestamp for easier comparison
        expectedJsonExceptionResponse.setTimestamp(0);

        given(userController.findAll()).willThrow(runtimeException);

        MvcResult mvcResult = mockMvc.perform(get(Endpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
                .andReturn();

        JsonExceptionResponse actualJsonExceptionResponse = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), JsonExceptionResponse.class);
        // Check timestamp is greater than current time
        assertThat(actualJsonExceptionResponse.getTimestamp() >= currentTimeInMillis);

        // Remove timestamp for easier comparison since it will be dynamic anyways
        actualJsonExceptionResponse.setTimestamp(0);

        assertThat(actualJsonExceptionResponse).isEqualTo(expectedJsonExceptionResponse);

        assertThat(mvcResult.getResolvedException()).isInstanceOf(Exception.class);
    }
}
