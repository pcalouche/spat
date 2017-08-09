package com.pcalouche.spat.config;

import com.pcalouche.spat.interceptors.AuthorizationInterceptor;
import com.pcalouche.spat.interceptors.LoggerInterceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.core.env.Environment;
import org.springframework.web.servlet.config.annotation.*;
import org.thymeleaf.spring4.SpringTemplateEngine;
import org.thymeleaf.spring4.templateresolver.SpringResourceTemplateResolver;
import org.thymeleaf.spring4.view.ThymeleafViewResolver;
import org.thymeleaf.templatemode.TemplateMode;

@Configuration
@EnableWebMvc
@EnableAspectJAutoProxy
public class SpatMvcConfig extends WebMvcConfigurerAdapter {
    private static final Logger logger = LoggerFactory.getLogger(SpatMvcConfig.class);
    private final Environment environment;
    private final AuthorizationInterceptor authorizationInterceptor;
    private final LoggerInterceptor loggerInterceptor;

    @Autowired
    public SpatMvcConfig(Environment environment, LoggerInterceptor loggerInterceptor, AuthorizationInterceptor authorizationInterceptor) {
        this.environment = environment;
        this.loggerInterceptor = loggerInterceptor;
        this.authorizationInterceptor = authorizationInterceptor;
    }

    @Override
    public void configureDefaultServletHandling(DefaultServletHandlerConfigurer configurer) {
        configurer.enable();
    }

    @Override
    public void configureViewResolvers(ViewResolverRegistry registry) {
        ThymeleafViewResolver thymeleafViewResolver = new ThymeleafViewResolver();
        thymeleafViewResolver.setTemplateEngine(templateEngine());
        registry.viewResolver(thymeleafViewResolver);
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/resources/dist/**")
                .addResourceLocations("/resources/dist/");
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggerInterceptor);
        registry.addInterceptor(authorizationInterceptor);
    }

    @Bean
    public SpringResourceTemplateResolver templateResolver() {
        SpringResourceTemplateResolver templateResolver = new SpringResourceTemplateResolver();
        templateResolver.setTemplateMode(TemplateMode.HTML);
        templateResolver.setPrefix("/resources/dist/");
        templateResolver.setSuffix(".html");
        String springThymeleafCache = environment.getProperty("spring.thymeleaf.cache");
        if (springThymeleafCache == null) {
            logger.info("No spring.thymeleaf.cache environment parameter set, so Thymeleaf templates will be cached.  Note this may NOT be desirable during development.");
            templateResolver.setCacheable(true);
        } else {
            if (Boolean.valueOf(springThymeleafCache)) {
                logger.info("spring.thymeleaf.cache environment parameter was set to true, so Thymeleaf templates will be cached.  Note this may NOT be desirable during development.");
                templateResolver.setCacheable(true);
            } else {
                logger.info("spring.thymeleaf.cache environment parameter was set to false, so Thymeleaf templates will NOT be cached.  Note this may NOT be desirable during production.");
                templateResolver.setCacheable(false);
            }
        }
        return templateResolver;
    }

    @Bean
    public SpringTemplateEngine templateEngine() {
        SpringTemplateEngine templateEngine = new SpringTemplateEngine();
        templateEngine.setEnableSpringELCompiler(true);
        templateEngine.setTemplateResolver(templateResolver());
        return templateEngine;
    }
}
