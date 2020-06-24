package com.pcalouche.spat.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SpringDocConfig {
    private final SpatProperties spatProperties;

    public SpringDocConfig(SpatProperties spatProperties) {
        this.spatProperties = spatProperties;
    }

    @Bean
    public OpenAPI openAPI() {
        return new OpenAPI()
                .info(new Info().title("SPAT API")
                        .description("Endpoints available in the SPAT API")
                        .version(spatProperties.getVersion())
                        .contact(new Contact()
                                .name("Philip Calouche")
                                .url("https://github.com/pcalouche/spat")
                                .email("philip.calouche@outlook.com"))
                );
    }
}