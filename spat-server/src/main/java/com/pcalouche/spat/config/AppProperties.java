package com.pcalouche.spat.config;

import org.hibernate.validator.constraints.NotEmpty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import javax.validation.Valid;

@ConfigurationProperties(prefix = "app")
@Validated
public class AppProperties {
    /**
     * my description 2
     */
    private String testProp;

    /**
     * Security config
     */
    @Valid
    private final Security security = new Security();

    public String getTestProp() {
        return testProp;
    }

    public void setTestProp(String testProp) {
        this.testProp = testProp;
    }

    public Security getSecurity() {
        return security;
    }

    public static class Security {
        /**
         * A username blah
         */
        @NotEmpty
        public String username;

        public String getUsername() {
            return username;
        }

        public void setUsername(String username) {
            this.username = username;
        }
    }

}
