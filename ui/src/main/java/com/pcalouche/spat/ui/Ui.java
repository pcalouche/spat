package com.pcalouche.spat.ui;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;

@SpringBootApplication
public class Ui extends SpringBootServletInitializer {
  public static void main(String[] args) {
    configureApplication(new SpringApplicationBuilder()).run(args);
  }

  private static SpringApplicationBuilder configureApplication(SpringApplicationBuilder builder) {
    return builder.sources(Ui.class);
  }

  @Override
  protected SpringApplicationBuilder configure(SpringApplicationBuilder builder) {
    return configureApplication(builder);
  }
}
