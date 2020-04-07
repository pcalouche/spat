package com.pcalouche.spat.api;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.api.controller.UserController;
import com.pcalouche.spat.util.ExceptionUtils;
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
        RuntimeException runtimeException = new RuntimeException("some random runtime exception");
        MockHttpServletRequest request = MockMvcRequestBuilders.get("/some-endpoint")
                .contentType(MediaType.APPLICATION_JSON)
                .buildRequest(new MockServletContext());
        request.setRequestURI(Endpoints.USERS);

        ObjectNode expectedObjectNode = (ObjectNode) ExceptionUtils.buildJsonErrorObject(runtimeException, request);
        // Remove timestamp for easier comparision
        expectedObjectNode.remove("timestamp");

        given(userController.findAll()).willThrow(runtimeException);

        MvcResult mvcResult = mockMvc.perform(get(Endpoints.USERS)
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
