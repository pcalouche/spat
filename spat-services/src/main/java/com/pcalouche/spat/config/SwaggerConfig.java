package com.pcalouche.spat.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

    @Bean
    public OpenAPI openAPI() {
        return new OpenAPI()
                .info(new Info().title("SPAT API")
                        .description("Endpoints available in the SPAT API")
                        .version("5.0")
                        .contact(new Contact()
                                .name("Philip Calouche")
                                .url("https://github.com/pcalouche/spat")
                                .email("philip.calouche@outlook.com"))
                );
    }
}