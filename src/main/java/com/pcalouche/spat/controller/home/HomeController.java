package com.pcalouche.spat.controller.home;

import com.pcalouche.spat.controller.AbstractController;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class HomeController extends AbstractController {
    @RequestMapping(value = HomeControllerUris.ROOT, method = RequestMethod.GET)
    public String index() {
        logger.info("Accessing Home Page");
        return "index";
    }
}
