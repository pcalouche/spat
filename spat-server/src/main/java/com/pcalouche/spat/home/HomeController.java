package com.pcalouche.spat.home;

import com.pcalouche.spat.util.LoggerUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;

@Controller
@RequestMapping(value = HomeUris.ROOT)
public class HomeController {
    @GetMapping
    public String home(HttpServletRequest request) {
        LoggerUtils.logInfo("Accessing Home Page");
        if (!request.getRequestURI().contains(".")) {
            return "forward:/ui/index.html";
        } else {
            return "/ui" + request.getRequestURI();
        }
    }
}