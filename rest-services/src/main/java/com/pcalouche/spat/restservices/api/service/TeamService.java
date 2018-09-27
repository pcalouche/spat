package com.pcalouche.spat.restservices.api.service;

import com.pcalouche.spat.restservices.api.dto.TeamDto;

import java.util.List;

public interface TeamService {
    TeamDto findById(Integer id);

    List<TeamDto> findAll();

    TeamDto save(TeamDto team);

    void delete(Integer id);
}
