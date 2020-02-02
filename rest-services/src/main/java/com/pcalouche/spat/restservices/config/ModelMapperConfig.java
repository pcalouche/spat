package com.pcalouche.spat.restservices.config;

import com.pcalouche.spat.restservices.api.dto.RoleDto;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.dto.UserDto;
import com.pcalouche.spat.restservices.entity.Role;
import com.pcalouche.spat.restservices.entity.Team;
import com.pcalouche.spat.restservices.entity.User;
import org.modelmapper.AbstractConverter;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.stream.Collectors;

@Configuration
public class ModelMapperConfig {

    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);

        Converter<Team, TeamDto> teamToTeamDtoConverter = new AbstractConverter<Team, TeamDto>() {
            @Override
            protected TeamDto convert(Team source) {
                return TeamDto.builder()
                        .id(source.getId())
                        .name(source.getName())
                        .build();
            }
        };

        modelMapper.addConverter(teamToTeamDtoConverter);

        Converter<Role, RoleDto> roleToRoleDtoConverter = new AbstractConverter<Role, RoleDto>() {
            @Override
            protected RoleDto convert(Role source) {
                return RoleDto.builder()
                        .id(source.getId())
                        .name(source.getName())
                        .build();
            }
        };

        modelMapper.addConverter(roleToRoleDtoConverter);

        Converter<User, UserDto> userToUserDtoConverter = new AbstractConverter<User, UserDto>() {
            @Override
            protected UserDto convert(User source) {
                return UserDto.builder()
                        .id(source.getId())
                        .username(source.getUsername())
                        .accountNonExpired(source.isAccountNonExpired())
                        .accountNonLocked(source.isAccountNonLocked())
                        .credentialsNonExpired(source.isCredentialsNonExpired())
                        .enabled(source.isEnabled())
                        .roleDtos(
                                source.getRoles().stream()
                                        .map(role -> modelMapper.map(role, RoleDto.class))
                                        .collect(Collectors.toSet())
                        )
                        .build();
            }
        };

        modelMapper.addConverter(userToUserDtoConverter);

        return modelMapper;
    }
}
