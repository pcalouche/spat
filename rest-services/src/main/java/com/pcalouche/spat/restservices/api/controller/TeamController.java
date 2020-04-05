package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.api.EndpointMessages;
import com.pcalouche.spat.restservices.api.Endpoints;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.dto.TeamEditRequest;
import com.pcalouche.spat.restservices.api.exception.RestResourceForbiddenException;
import com.pcalouche.spat.restservices.api.exception.RestResourceNotFoundException;
import com.pcalouche.spat.restservices.service.TeamService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Team endpoints")
@RestController
@RequestMapping(value = Endpoints.TEAMS)
public class TeamController {
    private final TeamService teamService;

    public TeamController(TeamService teamService) {
        this.teamService = teamService;
    }

    @Operation(description = "Find all teams")
    @GetMapping
    public List<TeamDto> findAll() {
        return teamService.findAll();
    }

    @Operation(description = "Create a new team")
    @PreAuthorize("hasAuthority('Admin')")
    @PostMapping
    public TeamDto create(@RequestBody TeamEditRequest teamEditRequest) {
        if (teamService.findByName(teamEditRequest.getName()).isPresent()) {
            throw new RestResourceForbiddenException(String.format(EndpointMessages.TEAM_ALREADY_EXISTS, teamEditRequest.getName()));
        }
        return teamService.create(teamEditRequest);
    }

    @Operation(description = "Update an existing team")
    @PreAuthorize("hasAuthority('Admin')")
    @PutMapping(value = "/{id}")
    public TeamDto update(@PathVariable Integer id, @RequestBody TeamEditRequest teamEditRequest) {
        return teamService.update(id, teamEditRequest)
                .orElseThrow(() -> new RestResourceNotFoundException(String.format(EndpointMessages.NO_TEAM_FOUND, id)));
    }

    @Operation(description = "Delete an existing team")
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
