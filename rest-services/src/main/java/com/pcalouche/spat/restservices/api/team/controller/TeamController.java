package com.pcalouche.spat.restservices.api.team.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.model.Team;
import com.pcalouche.spat.restservices.api.team.service.TeamService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = TeamEndpoints.ROOT)
public class TeamController extends AbstractSpatController {
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
        logger.debug("Team to save name is " + team.getName() + " " + team.getId());
        return teamService.saveTeam(team);
    }

    @DeleteMapping(value = "/{id}")
    public boolean deleteTeam(@PathVariable Long id) {
        logger.debug("ID to delete from controller is " + id);
        return teamService.deleteTeam(id);
    }
}
