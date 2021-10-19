package com.pcalouche.spat.api.controller;

import com.pcalouche.spat.api.EndpointMessages;
import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.api.dto.TeamDto;
import com.pcalouche.spat.api.dto.TeamEditRequest;
import com.pcalouche.spat.exception.ApiErrorResponse;
import com.pcalouche.spat.exception.ApiForbiddenException;
import com.pcalouche.spat.exception.ApiNotFoundException;
import com.pcalouche.spat.service.TeamService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
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
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "get all teams")})
    @GetMapping
    public List<TeamDto> findAll() {
        return teamService.findAll();
    }

    @Operation(description = "Create a new team")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "create team"),
            @ApiResponse(
                    responseCode = "403",
                    description = "team already exists",
                    content = @Content(schema = @Schema(implementation = ApiErrorResponse.class))
            )
    })
    @PreAuthorize("hasAuthority('Admin')")
    @PostMapping
    public TeamDto create(@RequestBody @Validated TeamEditRequest teamEditRequest) {
        teamService.findByName(teamEditRequest.getName())
                .ifPresent(teamDto -> {
                    throw new ApiForbiddenException(String.format(EndpointMessages.TEAM_ALREADY_EXISTS, teamDto.getName()));
                });
        return teamService.create(teamEditRequest);
    }

    @Operation(description = "Update an existing team")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "update team"),
            @ApiResponse(
                    responseCode = "404",
                    description = "team not found",
                    content = @Content(schema = @Schema(implementation = ApiErrorResponse.class))
            )
    })
    @PreAuthorize("hasAuthority('Admin')")
    @PutMapping(value = "/{id}")
    public TeamDto update(@PathVariable Integer id, @RequestBody @Validated TeamEditRequest teamEditRequest) {
        return teamService.update(id, teamEditRequest)
                .orElseThrow(() -> new ApiNotFoundException(String.format(EndpointMessages.NO_TEAM_FOUND, id)));
    }

    @Operation(description = "Delete an existing team")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "delete team"),
            @ApiResponse(
                    responseCode = "404",
                    description = "team not found",
                    content = @Content(schema = @Schema(implementation = ApiErrorResponse.class))
            )
    })
    @PreAuthorize("hasAuthority('Admin')")
    @DeleteMapping(value = "/{id}")
    public void delete(@PathVariable Integer id) {
        teamService.findById(id).ifPresentOrElse(
                teamDto -> teamService.delete(teamDto.getId()),
                () -> {
                    throw new ApiNotFoundException(String.format(EndpointMessages.NO_TEAM_FOUND, id));
                }
        );
    }
}
