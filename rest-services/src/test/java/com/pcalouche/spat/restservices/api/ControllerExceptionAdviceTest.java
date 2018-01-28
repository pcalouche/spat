package com.pcalouche.spat.restservices.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.api.user.controller.UserEndpoints;
import com.pcalouche.spat.restservices.util.ExceptionUtils;
import org.junit.Before;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = UserController.class)
@WithMockUser
public class ControllerExceptionAdviceTest extends AbstractControllerTest {
    @MockBean
    private UserController userController;
    private MockHttpServletRequestBuilder request;

    @Before
    public void beforeLocal() {
        request = MockMvcRequestBuilders.get(UserEndpoints.ROOT);
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
}
