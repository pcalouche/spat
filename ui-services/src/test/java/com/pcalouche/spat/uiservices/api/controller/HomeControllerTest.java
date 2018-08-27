package com.pcalouche.spat.uiservices.api.controller;

import com.pcalouche.spat.uiservices.AbstractControllerTest;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;

@WebMvcTest(value = HomeController.class)
public class HomeControllerTest extends AbstractControllerTest {

    @Test
    public void testAngularHome() throws Exception {
        //        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(ApiEndpoints.ANGULAR);
        //        MvcResult mvcResult = mockMvc.perform(request)
        //                .andExpect(status().isOk())
        //                .andReturn();
        //
        //        ModelAndView mv = mvcResult.getModelAndView();
        //        assertThat(mv.getViewName()).isEqualTo("forward:angular/index.html");
    }

    @Test
    public void testReactHome() throws Exception {
        //        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(ApiEndpoints.REACT);
        //        MvcResult mvcResult = mockMvc.perform(request)
        //                .andExpect(status().isOk())
        //                .andReturn();
        //
        //        ModelAndView mv = mvcResult.getModelAndView();
        //        assertThat(mv.getViewName()).isEqualTo("forward:react/index.html");
    }
}
