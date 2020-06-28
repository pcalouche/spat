package com.pcalouche.spat.service;

import com.pcalouche.spat.api.dto.TeamDto;
import com.pcalouche.spat.api.dto.TeamEditRequest;
import com.pcalouche.spat.entity.Team;
import com.pcalouche.spat.repository.TeamRepository;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class TeamServiceImpl implements TeamService {
    private final TeamRepository teamRepository;

    public TeamServiceImpl(TeamRepository teamRepository) {
        this.teamRepository = teamRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public Optional<TeamDto> findById(Integer id) {
        return teamRepository.findById(id)
                .map(TeamDto::map);
    }

    @Override
    public Optional<TeamDto> findByName(String name) {
        return teamRepository.findByName(name)
                .map(TeamDto::map);
    }

    @Override
    @Transactional(readOnly = true)
    public List<TeamDto> findAll() {
        return teamRepository.findAll(Sort.by("name")).stream()
                .map(TeamDto::map)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional
    public TeamDto create(TeamEditRequest teamEditRequest) {
        Team team = teamRepository.save(
                Team.builder()
                        .name(teamEditRequest.getName())
                        .build()
        );
        return TeamDto.map(team);
    }

    @Override
    @Transactional
    public Optional<TeamDto> update(int id, TeamEditRequest teamEditRequest) {
        return teamRepository.findById(id)
                .map(team -> {
                    team.setName(teamEditRequest.getName());
                    return TeamDto.map(teamRepository.save(team));
                });
    }

    @Override
    @Transactional
    public void delete(Integer id) {
        teamRepository.deleteById(id);
    }
}
