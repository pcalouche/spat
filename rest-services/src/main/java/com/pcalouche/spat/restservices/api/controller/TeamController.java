package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.EndpointMessages;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.exception.RestResourceNotFoundException;
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

    @ApiOperation(value = "Find all teams")
    @GetMapping
    public List<TeamDto> findAll() {
        return teamService.findAll();
    }

    @ApiOperation(value = "Create a new team")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PostMapping
    public TeamDto create(@RequestBody TeamDto teamDto) {
        // Ensure id is null, since this should be for creation
        teamDto.setId(null);
        return teamService.save(teamDto);
    }

    @ApiOperation(value = "Update an existing team")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @PutMapping(value = "/{id}")
    public TeamDto update(@PathVariable Integer id, @RequestBody TeamDto teamDto) {
        if (teamService.findById(id) == null) {
            throw new RestResourceNotFoundException(String.format(EndpointMessages.NO_TEAM_FOUND, id));
        }
        // Make sure id was not changed in payload and the wrong record gets updates
        teamDto.setId(id);
        return teamService.save(teamDto);
    }

    @ApiOperation(value = "Delete an existing team")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    @DeleteMapping(value = "/{id}")
    public void delete(@PathVariable Integer id) {
        if (teamService.findById(id) == null) {
            throw new RestResourceNotFoundException(String.format(EndpointMessages.NO_TEAM_FOUND, id));
        }
        teamService.delete(id);
    }
}
