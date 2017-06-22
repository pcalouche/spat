package com.pcalouche.spat.config;

import com.pcalouche.spat.interceptors.AuthorizationInterceptor;
import com.pcalouche.spat.interceptors.LoggerInterceptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.*;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.servlet.config.annotation.*;

@Configuration
@Import({
        DatabaseConfig.class,
        ViewConfig.class
})
@EnableWebMvc
@EnableAspectJAutoProxy
@EnableTransactionManagement
@ComponentScan("com.pcalouche.spat")
@PropertySources({
        @PropertySource("classpath:database.properties")
})
public class SpatConfig extends WebMvcConfigurerAdapter {
    private final AuthorizationInterceptor authorizationInterceptor;
    private final LoggerInterceptor loggerInterceptor;

    @Autowired
    public SpatConfig(LoggerInterceptor loggerInterceptor, AuthorizationInterceptor authorizationInterceptor) {
        this.loggerInterceptor = loggerInterceptor;
        this.authorizationInterceptor = authorizationInterceptor;
    }

    @Bean
    public static PropertySourcesPlaceholderConfigurer propertyConfigInDev() {
        return new PropertySourcesPlaceholderConfigurer();
    }

    @Override
    public void configureDefaultServletHandling(DefaultServletHandlerConfigurer configurer) {
        configurer.enable();
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry
                .addResourceHandler("/resources/dist/**")
                .addResourceLocations("/resources/dist/");
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggerInterceptor);
        registry.addInterceptor(authorizationInterceptor);
    }
}