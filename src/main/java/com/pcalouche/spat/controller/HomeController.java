package com.pcalouche.spat.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class HomeController {
    private static final Logger logger = LoggerFactory.getLogger(HomeController.class);

    @RequestMapping(value = HomeControllerUris.ROOT, method = RequestMethod.GET)
    public String index() {
        logger.info("Accessing Home Page");
        return "index";
    }
}
