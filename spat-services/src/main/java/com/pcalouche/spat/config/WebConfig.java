package com.pcalouche.spat.config;

import com.pcalouche.spat.interceptors.LoggerInterceptor;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfig implements WebMvcConfigurer {
    private final SpatProperties spatProperties;
    private final LoggerInterceptor loggerInterceptor;

    public WebConfig(SpatProperties spatProperties,
                     LoggerInterceptor loggerInterceptor) {
        this.spatProperties = spatProperties;
        this.loggerInterceptor = loggerInterceptor;
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggerInterceptor);
    }

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOriginPatterns(spatProperties.getCorsAllowedOrigins())
                .allowCredentials(true)
                .allowedHeaders("*")
                .allowedMethods(
                        "OPTIONS",
                        "GET",
                        "POST",
                        "PUT",
                        "PATCH",
                        "DELETE"
                );
    }
}
