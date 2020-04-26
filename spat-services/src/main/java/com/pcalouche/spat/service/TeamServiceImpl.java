package com.pcalouche.spat.service;

import com.pcalouche.spat.api.dto.TeamDto;
import com.pcalouche.spat.api.dto.TeamEditRequest;
import com.pcalouche.spat.entity.Team;
import com.pcalouche.spat.repository.TeamRepository;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Sort;
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
        return teamRepository.findById(id)
                .map(team -> modelMapper.map(team, TeamDto.class));
    }

    @Override
    public Optional<TeamDto> findByName(String name) {
        return teamRepository.findByName(name)
                .map(team -> modelMapper.map(team, TeamDto.class));
    }

    @Override
    @Transactional(readOnly = true)
    public List<TeamDto> findAll() {
        return teamRepository.findAll(Sort.by("name")).stream()
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
