package com.calouche.spat.controller;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.servlet.ModelAndView;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class HomeControllerTest {
    private final HomeController homeController = new HomeController();
    private final MockMvc mockMvc = MockMvcBuilders.standaloneSetup(homeController).build();

    @Test
    public void homeDefaultPathTest() throws Exception {
        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(HomeControllerUris.ROOT);
        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        ModelAndView mv = mvcResult.getModelAndView();
        Assert.assertEquals(mv.getModel().get("environment"), "dev");
        Assert.assertEquals(mv.getModel().get("version"), "1.0");

        Assert.assertEquals(mv.getViewName(), "index");
    }

    @Test
    public void homeBogusPathTest() throws Exception {
        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get("/bogus");
        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        ModelAndView mv = mvcResult.getModelAndView();
        Assert.assertEquals(mv.getModel().get("environment"), "dev");
        Assert.assertEquals(mv.getModel().get("version"), "1.0");

        Assert.assertEquals(mv.getViewName(), "index");
    }
}