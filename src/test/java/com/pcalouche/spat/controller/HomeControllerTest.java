package com.pcalouche.spat.controller;

import com.pcalouche.spat.ControllerTest;
import com.pcalouche.spat.controller.home.HomeController;
import com.pcalouche.spat.controller.home.HomeControllerUris;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

@WebMvcTest(controllers = {HomeController.class})
public class HomeControllerTest extends ControllerTest {
    @Test
    public void homeDefaultPathTest() throws Exception {
        mockMvc.perform(get(HomeControllerUris.ROOT))
                .andExpect(status().isOk())
                .andExpect(view().name("index"))
                .andReturn();
    }

    @Test
    public void homeBogusPathTest() throws Exception {
        mockMvc.perform(get("/bogus"))
                .andExpect(status().isOk())
                .andExpect(view().name("index"))
                .andReturn();
    }
}