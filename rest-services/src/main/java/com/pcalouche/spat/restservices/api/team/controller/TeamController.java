package com.pcalouche.spat.restservices.api.team.controller;

import com.pcalouche.spat.restservices.api.model.Team;
import com.pcalouche.spat.restservices.api.team.service.TeamService;
import com.pcalouche.spat.restservices.util.LoggerUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = TeamEndpoints.ROOT)
public class TeamController {
    private final TeamService teamService;

    @Autowired
    public TeamController(TeamService teamService) {
        this.teamService = teamService;
    }

    @GetMapping
    public List<Team> getTeams() {
        return teamService.getTeams();
    }

    @PostMapping
    public Team saveTeam(@RequestBody Team team) {
        LoggerUtils.logDebug("Team to save name is " + team.getName() + " " + team.getId());
        return teamService.saveTeam(team);
    }

    @DeleteMapping(value = "/{id}")
    public ResponseEntity<Boolean> deleteTeam(@PathVariable Long id) {
        LoggerUtils.logDebug("ID to delete from controller is " + id);
        return new ResponseEntity<>(teamService.deleteTeam(id), HttpStatus.OK);
    }
}
