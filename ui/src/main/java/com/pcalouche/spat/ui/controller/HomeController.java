package com.pcalouche.spat.ui.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping
public class HomeController {

    @GetMapping(value = HomeEndpoints.ROOT)
    public String home() {
        return "forward:index.html";
    }
}
