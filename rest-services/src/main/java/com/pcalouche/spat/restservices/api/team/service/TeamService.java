package com.pcalouche.spat.restservices.api.team.service;

import com.pcalouche.spat.restservices.api.dto.TeamDto;

import java.util.List;

public interface TeamService {
    List<TeamDto> findAll();

    TeamDto save(TeamDto team);

    Boolean delete(Long id);
}
