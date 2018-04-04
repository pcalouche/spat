package com.pcalouche.spat.restservices.config;

import com.pcalouche.spat.restservices.interceptors.LoggerInterceptor;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfig implements WebMvcConfigurer {
    private final LoggerInterceptor loggerInterceptor;

    public WebConfig(LoggerInterceptor loggerInterceptor) {
        this.loggerInterceptor = loggerInterceptor;
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggerInterceptor);
    }

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedMethods("OPTIONS",
                        "GET",
                        "POST",
                        "PUT",
                        "DELETE")
                .exposedHeaders(HttpHeaders.LOCATION,
                        HttpHeaders.CONTENT_LOCATION,
                        HttpHeaders.CONTENT_TYPE,
                        HttpHeaders.CONTENT_LENGTH,
                        HttpHeaders.CONTENT_DISPOSITION)
                .allowCredentials(true)
                .maxAge(86400);
    }
}
