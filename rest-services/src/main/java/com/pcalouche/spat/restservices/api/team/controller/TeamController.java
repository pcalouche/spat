package com.pcalouche.spat.restservices.api.team.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.team.service.TeamService;
import org.modelmapper.ModelMapper;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping(value = TeamEndpoints.ROOT)
public class TeamController extends AbstractSpatController {
    private final TeamService teamService;
    private final ModelMapper modelMapper;

    public TeamController(TeamService teamService,
                          ModelMapper modelMapper) {
        this.teamService = teamService;
        this.modelMapper = modelMapper;
    }

    @GetMapping
    public List<TeamDto> getTeams() {
        return teamService.getTeams()
                .stream()
                .map(team -> modelMapper.map(team, TeamDto.class))
                .collect(Collectors.toList());
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PostMapping
    public TeamDto saveTeam(@RequestBody TeamDto teamDto) {
        Team team = modelMapper.map(teamDto, Team.class);
        return modelMapper.map(teamService.saveTeam(team), TeamDto.class);
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @DeleteMapping(value = "/{id}")
    public boolean deleteTeam(@PathVariable Long id) {
        return teamService.deleteTeam(id);
    }
}
