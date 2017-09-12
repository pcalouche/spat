package com.pcalouche.spat.controller;

import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.controller.team.TeamController;
import com.pcalouche.spat.controller.team.TeamControllerUris;
import com.pcalouche.spat.model.Team;
import com.pcalouche.spat.service.team.TeamService;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = TeamController.class)
public class TeamControllerTest extends AbstractControllerTest {
    @MockBean
    private TeamService teamService;

    @Test
    public void testGetTeams() throws Exception {
        List<Team> expectedTeams = new ArrayList<>();
        expectedTeams.add(new Team(1L, "Team1"));
        expectedTeams.add(new Team(2L, "Team2"));

        given(teamService.getTeams()).willReturn(expectedTeams);

        mockMvc.perform(get(TeamControllerUris.ROOT))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(content().json(objectMapper.writeValueAsString(expectedTeams)));

        verify(teamService, times(1)).getTeams();
    }

    @Test
    public void testSaveTeam() throws Exception {
        Team expectedTeam = new Team(1L, "Team1");

        given(teamService.saveTeam(expectedTeam)).willReturn(expectedTeam);

        MockHttpServletRequestBuilder request = post(TeamControllerUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedTeam));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(content().json(objectMapper.writeValueAsString(expectedTeam)));

        verify(teamService, times(1)).saveTeam(expectedTeam);
    }

    @Test
    public void testDeleteTeam() throws Exception {
        given(teamService.deleteTeam(1L)).willReturn(true);

        mockMvc.perform(delete(String.format("%s/%d", TeamControllerUris.ROOT, 1L)))
                .andExpect(status().isOk())
                .andExpect(content().string(Boolean.TRUE.toString()));

        verify(teamService, times(1)).deleteTeam(1L);
    }

    @Test
    public void testDeleteTeamNotFound() throws Exception {
        given(teamService.deleteTeam(1L)).willReturn(false);

        mockMvc.perform(delete(String.format("%s/%d", TeamControllerUris.ROOT, 1L)))
                .andExpect(status().isOk())
                .andExpect(content().string(Boolean.FALSE.toString()));

        verify(teamService, times(1)).deleteTeam(1L);
    }
}