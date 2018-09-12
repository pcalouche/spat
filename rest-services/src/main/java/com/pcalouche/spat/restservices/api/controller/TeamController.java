package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.service.TeamService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Api(description = "Team endpoints")
@RestController
@RequestMapping(value = ApiEndpoints.TEAMS)
public class TeamController extends AbstractSpatController {
    private final TeamService teamService;

    public TeamController(TeamService teamService) {
        this.teamService = teamService;
    }

    @ApiOperation(value = "Use to find all teams")
    @GetMapping
    public List<TeamDto> findAll() {
        return teamService.findAll();
    }

    @ApiOperation(value = "Use to save a team")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PostMapping
    public TeamDto save(@RequestBody TeamDto teamDto) {
        return teamService.save(teamDto);
    }

    @ApiOperation(value = "Use to delete a team")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @DeleteMapping(value = "/{id}")
    public void deleteById(@PathVariable Long id) {
        teamService.deleteById(id);
    }
}
