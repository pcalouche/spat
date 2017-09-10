package com.pcalouche.spat.controller.team;

import com.pcalouche.spat.controller.AbstractController;
import com.pcalouche.spat.model.Team;
import com.pcalouche.spat.service.team.TeamService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.naming.AuthenticationException;
import java.util.List;

@RestController
@RequestMapping(value = TeamControllerUris.ROOT)
public class TeamController extends AbstractController {
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
