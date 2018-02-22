package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.api.entity.Team;
import org.modelmapper.ModelMapper;

import java.util.Objects;

public class TeamDto implements EntityConvertible<Team> {
    private Long id;
    private String name;

    public TeamDto() {
    }

    public TeamDto(Long id, String name) {
        this.id = id;
        this.name = name;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public Team convertToEntity(ModelMapper modelMapper) {
        return modelMapper.map(this, Team.class);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        TeamDto teamDto = (TeamDto) o;
        return Objects.equals(name, teamDto.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
