package com.pcalouche.spat.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

@Configuration
@Import({
        SpatPropertySourcesConfig.class,
        DatabaseConfig.class,
        SpatMvcConfig.class
})
@ComponentScan("com.pcalouche.spat")
public class SpatConfig extends WebMvcConfigurerAdapter {
}