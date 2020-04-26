package com.pcalouche.spat.config;

import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.time.Duration;

@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "spat")
@Validated
public class SpatProperties {
    /**
     * What origins to allow access from
     */
    private String[] corsAllowedOrigins;
    /**
     * The signing key used for JWTs.
     */
    @NotEmpty
    private String jwtSigningKey;
    /**
     * How long JWT tokens are good for
     */
    @NonNull
    private Duration jwtTokenDuration;
    /**
     * How long JWT refresh tokens are good for
     */
    @NotNull
    private Duration refreshTokenDuration;
    /**
     * The hostname of the environment
     */
    @NotEmpty
    private String hostname;

    public boolean isHttpsEnvironment() {
        return !"localhost".equals(hostname);
    }
}