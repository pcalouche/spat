package com.pcalouche.spat.restservices.service;

import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.dto.TeamEditRequest;

import java.util.List;
import java.util.Optional;

public interface TeamService {
    Optional<TeamDto> findById(Integer id);

    List<TeamDto> findAll();

    TeamDto create(TeamEditRequest teamEditRequest);

    Optional<TeamDto> update(int id, TeamEditRequest teamEditRequest);

    void delete(Integer id);
}
