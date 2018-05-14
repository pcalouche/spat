package com.pcalouche.spat.restservices.api;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.user.controller.UserController;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;

@WebMvcTest(value = UserController.class)
public class ControllerExceptionAdviceTest extends AbstractControllerTest {
    @MockBean
    private UserController userController;

    @Test
    public void testException() throws Exception {
//        RuntimeException runtimeException = new RuntimeException("some random runtime exception");
//        MockHttpServletRequest request = new MockHttpServletRequest();
//        request.setRequestURI(UserEndpoints.ROOT);
//
//        ObjectNode expectedObjectNode = (ObjectNode) ExceptionUtils.buildJsonErrorObject(runtimeException, request);
//        // Remove timestamp for easier comparision
//        expectedObjectNode.remove("timestamp");
//
//        given(userController.getUsers()).willThrow(runtimeException);
//
//        MvcResult mvcResult = mockMvc.perform(get(UserEndpoints.ROOT)
//                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
//                .andExpect(status().is(HttpStatus.INTERNAL_SERVER_ERROR.value()))
//                .andReturn();
//
//        ObjectNode actualObjectNode = (ObjectNode) objectMapper.readTree(mvcResult.getResponse().getContentAsString());
//        // Check timestamp is not null
//        assertThat(actualObjectNode.get("timestamp"))
//                .isNotNull();
//
//        // Remove timestamp for easier comparision
//        actualObjectNode.remove("timestamp");
//
//        assertThat(actualObjectNode).isEqualTo(expectedObjectNode);
//
//        assertThat(mvcResult.getResolvedException()).isInstanceOf(Exception.class);
    }
}
