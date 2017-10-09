package com.pcalouche.spat.controller;

import com.pcalouche.spat.controller.home.HomeController;
import com.pcalouche.spat.controller.home.HomeUris;
import org.junit.Test;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.servlet.ModelAndView;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class HomeControllerTest {
    private final HomeController homeController = new HomeController();
    private final MockMvc mockMvc = MockMvcBuilders.standaloneSetup(homeController).build();

    @Test
    public void homeDefaultPathTest() throws Exception {
        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(HomeUris.ROOT);
        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        ModelAndView mv = mvcResult.getModelAndView();
        assertThat(mv.getViewName()).isEqualTo("forward:/dist/index.html");
    }

    @Test
    public void homeBogusPathTest() throws Exception {
        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get("/bogus");
        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        ModelAndView mv = mvcResult.getModelAndView();

        assertThat(mv.getViewName()).isEqualTo("forward:/dist/index.html");
    }
}
