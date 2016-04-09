package com.calouche.spat.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class HomeController {
    private static final Logger logger = LoggerFactory.getLogger(HomeController.class);

    @RequestMapping(value = "/*", method = RequestMethod.GET)
    public String home(ModelMap model) {
        logger.info("Accessing Home Page");
        // Simulating get some server side data for AngularJS frontend
        model.addAttribute("environment", "dev");
        model.addAttribute("version", "1.0");
        return "index";
    }
}
