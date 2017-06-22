package com.pcalouche.spat.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.web.servlet.ViewResolver;
import org.thymeleaf.spring4.SpringTemplateEngine;
import org.thymeleaf.spring4.templateresolver.SpringResourceTemplateResolver;
import org.thymeleaf.spring4.view.ThymeleafViewResolver;
import org.thymeleaf.templatemode.TemplateMode;

@Configuration
public class ViewConfig {
    private static final Logger logger = LoggerFactory.getLogger(ViewConfig.class);
    private final Environment environment;

    @Autowired
    public ViewConfig(Environment environment) {
        this.environment = environment;
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

    @Bean
    public ViewResolver viewResolver() {
        ThymeleafViewResolver viewResolver = new ThymeleafViewResolver();
        viewResolver.setTemplateEngine(templateEngine());
        return viewResolver;
    }
}
