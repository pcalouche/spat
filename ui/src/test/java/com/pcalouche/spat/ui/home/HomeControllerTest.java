package com.pcalouche.spat.ui.home;

import com.pcalouche.spat.ui.controller.HomeController;
import com.pcalouche.spat.ui.controller.HomeEndpoints;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.web.servlet.ModelAndView;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = HomeController.class)
public class HomeControllerTest extends AbstractControllerTest {

    @Test
    public void homeDefaultPathTest() throws Exception {
        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(HomeEndpoints.ROOT);
        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        ModelAndView mv = mvcResult.getModelAndView();
        assertThat(mv.getViewName()).isEqualTo("forward:index.html");
    }
}
