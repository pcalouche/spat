package com.pcalouche.spat.home;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;

@Controller
@RequestMapping
public class HomeController {
    @GetMapping(value = HomeUris.ROOT)
    public String home(HttpServletRequest request) {
        return "forward:index.html";
    }
}