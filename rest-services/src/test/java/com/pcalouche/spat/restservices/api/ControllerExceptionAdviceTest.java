package com.pcalouche.spat.restservices.api;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.controller.UserController;
import com.pcalouche.spat.restservices.util.ExceptionUtils;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.web.servlet.MvcResult;

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
        RuntimeException runtimeException = new RuntimeException("some random runtime exception");
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setRequestURI(ApiEndpoints.USERS);

        ObjectNode expectedObjectNode = (ObjectNode) ExceptionUtils.buildJsonErrorObject(runtimeException, request);
        // Remove timestamp for easier comparision
        expectedObjectNode.remove("timestamp");

        given(userController.findAll()).willThrow(runtimeException);

        MvcResult mvcResult = mockMvc.perform(get(ApiEndpoints.USERS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
                .andReturn();

        ObjectNode actualObjectNode = (ObjectNode) objectMapper.readTree(mvcResult.getResponse().getContentAsString());
        // Check timestamp is not null
        assertThat(actualObjectNode.get("timestamp"))
                .isNotNull();

        // Remove timestamp for easier comparision since it will be dynamic anyways
        actualObjectNode.remove("timestamp");

        assertThat(actualObjectNode).isEqualTo(expectedObjectNode);

        assertThat(mvcResult.getResolvedException()).isInstanceOf(Exception.class);
    }
}
