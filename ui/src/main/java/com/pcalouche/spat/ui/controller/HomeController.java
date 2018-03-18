package com.pcalouche.spat.ui.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;

@Controller
@RequestMapping
public class HomeController {

  @GetMapping(value = HomeEndpoints.ROOT)
  public String home(HttpServletRequest request) {
    return "forward:index.html";
  }
}
