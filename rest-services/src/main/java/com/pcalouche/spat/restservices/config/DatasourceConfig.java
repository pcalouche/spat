package com.pcalouche.spat.restservices.config;

import org.h2.tools.Server;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.sql.SQLException;

@Configuration
public class DatasourceConfig {
    /**
     * Start internal H2 server so we can query the DB from IDE
     *
     * @return H2 Server instance
     * @throws SQLException sql exception
     */
    @Bean(initMethod = "start", destroyMethod = "stop")
    public Server h2Server() throws SQLException {
        return Server.createTcpServer("-tcp", "-tcpAllowOthers", "-tcpPort", "9092");
    }

}
