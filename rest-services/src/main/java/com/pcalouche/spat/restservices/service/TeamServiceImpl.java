package com.pcalouche.spat.restservices.service;

import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.dto.TeamEditRequest;
import com.pcalouche.spat.restservices.entity.Team;
import com.pcalouche.spat.restservices.repository.TeamRepository;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class TeamServiceImpl implements TeamService {
    private final ModelMapper modelMapper;
    private final TeamRepository teamRepository;

    public TeamServiceImpl(ModelMapper modelMapper, TeamRepository teamRepository) {
        this.modelMapper = modelMapper;
        this.teamRepository = teamRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public Optional<TeamDto> findById(Integer id) {
        Optional<TeamDto> teamDtoOptional = Optional.empty();
        Optional<Team> optionalTeam = teamRepository.findById(id);
        if (optionalTeam.isPresent()) {
            teamDtoOptional = Optional.of(modelMapper.map(optionalTeam.get(), TeamDto.class));
        }
        return teamDtoOptional;
    }

    @Override
    @Transactional(readOnly = true)
    public List<TeamDto> findAll() {
        return teamRepository.findAll().stream()
                .map(team -> modelMapper.map(team, TeamDto.class))
                .collect(Collectors.toList());
    }

    @Override
    @Transactional
    public TeamDto create(TeamEditRequest teamEditRequest) {
        Team team = Team.builder()
                .name(teamEditRequest.getName())
                .build();
        return modelMapper.map(teamRepository.save(team), TeamDto.class);
    }

    @Override
    @Transactional
    public Optional<TeamDto> update(int id, TeamEditRequest teamEditRequest) {
        Optional<TeamDto> teamDtoOptional = Optional.empty();
        Optional<Team> optionalTeam = teamRepository.findById(id);
        if (optionalTeam.isPresent()) {
            Team team = optionalTeam.get();
            team.setName(teamEditRequest.getName());
            teamDtoOptional = Optional.of(modelMapper.map(teamRepository.save(team), TeamDto.class));
        }
        return teamDtoOptional;
    }

    @Override
    @Transactional
    public void delete(Integer id) {
        teamRepository.deleteById(id);
    }
}
