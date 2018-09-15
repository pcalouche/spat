package com.pcalouche.spat.uiservices.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.resource.PathResourceResolver;

import java.io.IOException;

@Configuration
public class WebConfig implements WebMvcConfigurer {

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/**")
                .addResourceLocations("classpath:/public/")
                .setCachePeriod(3600)
                .resourceChain(true)
                .addResolver(new PathResourceResolver() {
                    @Override
                    protected Resource getResource(String resourcePath,
                                                   Resource location) throws IOException {
                        Resource requestedResource = location.createRelative(resourcePath);
                        if (requestedResource.exists() && requestedResource.isReadable()) {
                            return requestedResource;
                        } else {
                            if (resourcePath.contains("angular")) {
                                return new ClassPathResource("/public/angular/index.html");
                            } else {
                                return new ClassPathResource("/public/react/index.html");
                            }
                        }
                        //                        else if ()
                        //                        return requestedResource.exists() && requestedResource.isReadable() ? requestedResource
                        //                                : new ClassPathResource("/public/index.html");
                    }
                });
        //
        //        registry.addResourceHandler("/angular/**")
        //                .addResourceLocations("classpath:/public/angular")
        //                .setCachePeriod(3600)
        //                .resourceChain(true)
        //                .addResolver(new PathResourceResolver() {
        //                    @Override
        //                    protected Resource getResource(String resourcePath,
        //                                                   Resource location) throws IOException {
        //                        Resource requestedResource = location.createRelative(resourcePath);
        //                        if (requestedResource.exists() && requestedResource.isReadable()) {
        //                            return requestedResource;
        //                        } else {
        //                            return new ClassPathResource("/public/angular/index.html");
        //                        }
        //                    }
        //                });
    }
}