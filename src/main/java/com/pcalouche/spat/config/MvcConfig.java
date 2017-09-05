package com.pcalouche.spat.config;

import com.pcalouche.spat.interceptors.AuthorizationInterceptor;
import com.pcalouche.spat.interceptors.LoggerInterceptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.DefaultServletHandlerConfigurer;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

@Configuration
public class MvcConfig extends WebMvcConfigurerAdapter {
    private final AuthorizationInterceptor authorizationInterceptor;
    private final LoggerInterceptor loggerInterceptor;

//    @Autowired
//    public MvcConfig(LoggerInterceptor loggerInterceptor) {
//        this.loggerInterceptor = loggerInterceptor;
//    }

    @Autowired
    public MvcConfig(LoggerInterceptor loggerInterceptor, AuthorizationInterceptor authorizationInterceptor) {
        this.loggerInterceptor = loggerInterceptor;
        this.authorizationInterceptor = authorizationInterceptor;
    }

//    @Autowired
//    public MvcConfig() {
//    }

    @Override
    public void configureDefaultServletHandling(DefaultServletHandlerConfigurer configurer) {
        configurer.enable();
    }

//    @Override
//    public void addResourceHandlers(ResourceHandlerRegistry registry) {
//        registry.addResourceHandler("/resources/static/dist/**")
//                .addResourceLocations("/resources/static/dist/");
//        registry.addResourceHandler("/resources/static/**")
//                .addResourceLocations("/resources/static/");
//        registry.addResourceHandler("/**")
//                .addResourceLocations("/resources/static/");
//    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggerInterceptor);
        registry.addInterceptor(authorizationInterceptor);
    }
//    @Override
//    public void addInterceptors(InterceptorRegistry registry) {
//        registry.addInterceptor(loggerInterceptor);
//    }
}
