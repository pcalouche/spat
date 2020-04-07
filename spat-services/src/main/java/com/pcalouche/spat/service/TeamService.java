package com.pcalouche.spat.service;

import com.pcalouche.spat.api.dto.TeamDto;
import com.pcalouche.spat.api.dto.TeamEditRequest;

import java.util.List;
import java.util.Optional;

public interface TeamService {
    Optional<TeamDto> findById(Integer id);

    Optional<TeamDto> findByName(String name);

    List<TeamDto> findAll();

    TeamDto create(TeamEditRequest teamEditRequest);

    Optional<TeamDto> update(int id, TeamEditRequest teamEditRequest);

    void delete(Integer id);
}
