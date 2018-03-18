package com.pcalouche.spat.restservices.config;

import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.api.entity.User;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.PropertyMap;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.List;
import java.util.stream.Collectors;

@Configuration
public class ModelMapperConfig {
    private final Converter<List<String>, List<SimpleGrantedAuthority>> authorityConverter = mappingContext -> mappingContext.getSource() == null ? null : mappingContext.getSource()
            .stream()
            .map(SimpleGrantedAuthority::new)
            .collect(Collectors.toList());

    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.getConfiguration()
                .setMatchingStrategy(MatchingStrategies.STRICT);
        modelMapper.addMappings(new PropertyMap<UserDto, User>() {
            @Override
            protected void configure() {
                using(authorityConverter).map(source.getAuthorities()).setAuthorities(null);
                skip().setPassword(null);
            }
        });
        return modelMapper;
    }
}
