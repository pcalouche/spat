package com.pcalouche.spat.config;

import com.pcalouche.spat.interceptors.LoggerInterceptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.DefaultServletHandlerConfigurer;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

@Configuration
@EnableConfigurationProperties(AppConfig.class)
public class MvcConfig extends WebMvcConfigurerAdapter {
    private final LoggerInterceptor loggerInterceptor;

    @Autowired
    public MvcConfig(LoggerInterceptor loggerInterceptor) {
        this.loggerInterceptor = loggerInterceptor;
    }

    @Override
    public void configureDefaultServletHandling(DefaultServletHandlerConfigurer configurer) {
        configurer.enable();
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggerInterceptor);
    }
}
