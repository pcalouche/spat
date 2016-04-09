package com.calouche.spat.config;

import com.calouche.spat.interceptors.AuthorizationInterceptor;
import com.calouche.spat.interceptors.LoggerInterceptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.*;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.web.servlet.config.annotation.DefaultServletHandlerConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.view.InternalResourceViewResolver;
import org.springframework.web.servlet.view.JstlView;

@EnableWebMvc
@EnableAspectJAutoProxy
@ComponentScan("com.calouche")
@PropertySource("classpath:database.properties")
@Configuration
public class SpatWebMvcConfigurerAdapter extends WebMvcConfigurerAdapter {

    @Autowired
    private Environment env;
    @Autowired
    private AuthorizationInterceptor authorizationInterceptor;
    @Autowired
    private LoggerInterceptor loggerInterceptor;

    @Override
    public void configureDefaultServletHandling(DefaultServletHandlerConfigurer configurer) {
        configurer.enable();
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggerInterceptor);
        registry.addInterceptor(authorizationInterceptor);
    }

    @Bean
    public InternalResourceViewResolver internalResourceViewResolver() {
        InternalResourceViewResolver internalResourceViewResolver = new InternalResourceViewResolver();
        internalResourceViewResolver.setPrefix("/WEB-INF/views/");
        internalResourceViewResolver.setSuffix(".jsp");
        internalResourceViewResolver.setViewClass(JstlView.class);
        return internalResourceViewResolver;
    }

    @Bean
    public DriverManagerDataSource dataSource() {
        DriverManagerDataSource driverManagerDataSource = new DriverManagerDataSource();
        driverManagerDataSource.setDriverClassName(env.getProperty("database.driverClassName"));
        driverManagerDataSource.setUrl(env.getProperty("database.url"));
        driverManagerDataSource.setUsername(env.getProperty("database.username"));
        driverManagerDataSource.setPassword(env.getProperty("database.password"));
        return driverManagerDataSource;
    }

}