package com.calouche.spat.controller;

import com.calouche.spat.model.Team;
import com.calouche.spat.service.team.TeamService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.naming.AuthenticationException;
import java.util.List;

@RestController
@RequestMapping(value = "/team")
public class TeamController {
    private static final Logger logger = LoggerFactory.getLogger(TeamController.class);
    private final TeamService teamService;

    @Autowired
    public TeamController(TeamService teamService) {
        this.teamService = teamService;
    }

    @RequestMapping(method = RequestMethod.GET)
    public List<Team> getTeams() throws AuthenticationException {
        return teamService.getTeams();
    }

    @RequestMapping(method = RequestMethod.POST)
    public Team saveTeam(@RequestBody Team team) {
        logger.debug("Team to save name is " + team.getName() + " " + team.getId());
        return teamService.saveTeam(team);
    }

    @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
    public ResponseEntity<Boolean> deleteTeam(@PathVariable Long id) {
        logger.info("ID to delete from controller is " + id);
        return new ResponseEntity<>(teamService.deleteTeam(id), HttpStatus.OK);
    }
}
