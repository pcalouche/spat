package com.pcalouche.spat.restservices.api.team.service;

import com.pcalouche.spat.restservices.api.AbstractSpatServiceImpl;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.team.repository.TeamRepository;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class TeamServiceImpl extends AbstractSpatServiceImpl implements TeamService {
    private final TeamRepository teamRepository;
    private final ModelMapper modelMapper;

    public TeamServiceImpl(TeamRepository teamRepository, ModelMapper modelMapper) {
        this.teamRepository = teamRepository;
        this.modelMapper = modelMapper;
    }

    @Override
    public List<TeamDto> findAll() {
        List<TeamDto> teamDtos = new ArrayList<>();
        teamRepository.findAll().forEach(team -> teamDtos.add(modelMapper.map(team, TeamDto.class)));
        return teamDtos;
    }

    @Override
    public TeamDto save(TeamDto teamDto) {
        Team team = modelMapper.map(teamDto, Team.class);
        return modelMapper.map(teamRepository.save(team), TeamDto.class);
    }

    @Override
    public Boolean delete(Long id) {
        teamRepository.deleteById(id);
        return true;
    }
}
