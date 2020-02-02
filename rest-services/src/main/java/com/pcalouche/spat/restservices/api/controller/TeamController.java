package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.EndpointMessages;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.dto.TeamEditRequest;
import com.pcalouche.spat.restservices.api.exception.RestResourceNotFoundException;
import com.pcalouche.spat.restservices.service.TeamService;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = ApiEndpoints.TEAMS)
public class TeamController {
    private final TeamService teamService;

    public TeamController(TeamService teamService) {
        this.teamService = teamService;
    }

    @ApiOperation(value = "Find all teams")
    @GetMapping
    public List<TeamDto> findAll() {
        return teamService.findAll();
    }

    @ApiOperation(value = "Create a new team")
    @PreAuthorize("hasAuthority('Admin')")
    @PostMapping
    public TeamDto create(@RequestBody TeamEditRequest teamEditRequest) {
        return teamService.create(teamEditRequest);
    }

    @ApiOperation(value = "Update an existing team")
    @PreAuthorize("hasAuthority('Admin')")
    @PutMapping(value = "/{id}")
    public TeamDto update(@PathVariable Integer id, @RequestBody TeamEditRequest teamEditRequest) {
        return teamService.update(id, teamEditRequest)
                .orElseThrow(() -> new RestResourceNotFoundException(String.format(EndpointMessages.NO_TEAM_FOUND, id)));
    }

    @ApiOperation(value = "Delete an existing team")
    @PreAuthorize("hasAuthority('Admin')")
    @DeleteMapping(value = "/{id}")
    public void delete(@PathVariable Integer id) {
        if (teamService.findById(id).isPresent()) {
            teamService.delete(id);
        } else {
            throw new RestResourceNotFoundException(String.format(EndpointMessages.NO_TEAM_FOUND, id));
        }
    }
}
