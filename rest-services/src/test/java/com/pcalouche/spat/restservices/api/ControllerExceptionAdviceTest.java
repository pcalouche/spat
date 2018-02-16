package com.pcalouche.spat.restservices.api;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import com.pcalouche.spat.restservices.api.user.controller.UserEndpoints;
import com.pcalouche.spat.restservices.util.ExceptionUtils;
import org.junit.Before;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
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
        MockHttpServletRequest mockRequest = new MockHttpServletRequest();
        mockRequest.setRequestURI(UserEndpoints.ROOT);

        ObjectNode expectedObjectNode = (ObjectNode) ExceptionUtils.buildJsonErrorObject(runtimeException, mockRequest);
        // Remove timestamp for easier comparision
        expectedObjectNode.remove("timestamp");

        given(userController.getUsers()).willThrow(runtimeException);

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
                .andReturn();

        ObjectNode actualObjectNode = (ObjectNode) objectMapper.readTree(mvcResult.getResponse().getContentAsString());
        // Check timestamp is not null
        assertThat(actualObjectNode.get("timestamp"))
                .isNotNull();

        // Remove timestamp for easier comparision
        actualObjectNode.remove("timestamp");

        assertThat(actualObjectNode).isEqualTo(expectedObjectNode);
        
        assertThat(mvcResult.getResolvedException()).isInstanceOf(Exception.class);
    }
}
