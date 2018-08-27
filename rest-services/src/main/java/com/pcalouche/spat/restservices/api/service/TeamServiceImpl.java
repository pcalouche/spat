package com.pcalouche.spat.restservices.api.service;

import com.pcalouche.spat.restservices.api.AbstractSpatServiceImpl;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.repository.TeamRepository;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class TeamServiceImpl extends AbstractSpatServiceImpl implements TeamService {
    private final ModelMapper modelMapper;
    private final TeamRepository teamRepository;

    public TeamServiceImpl(ModelMapper modelMapper, TeamRepository teamRepository) {
        this.modelMapper = modelMapper;
        this.teamRepository = teamRepository;
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
    public void deleteById(Long id) {
        teamRepository.deleteById(id);
    }
}
