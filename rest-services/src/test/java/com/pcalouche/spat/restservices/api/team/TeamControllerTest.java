package com.pcalouche.spat.restservices.api.team;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.model.Team;
import com.pcalouche.spat.restservices.api.team.controller.TeamController;
import com.pcalouche.spat.restservices.api.team.controller.TeamEndpoints;
import com.pcalouche.spat.restservices.api.team.service.TeamService;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.util.ArrayList;
import java.util.List;

@WebMvcTest(value = TeamController.class)
public class TeamControllerTest extends AbstractControllerTest {
    @MockBean
    private TeamService teamService;

    @Test
    public void testGetTeams() throws Exception {
        List<Team> expectedTeams = new ArrayList<>();
        expectedTeams.add(new Team(1L, "Team1"));
        expectedTeams.add(new Team(2L, "Team2"));

        BDDMockito.given(teamService.getTeams()).willReturn(expectedTeams);

        mockMvc.perform(MockMvcRequestBuilders.get(TeamEndpoints.ROOT))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(MockMvcResultMatchers.content().json(objectMapper.writeValueAsString(expectedTeams)));

        Mockito.verify(teamService, Mockito.times(1)).getTeams();
    }

    @Test
    public void testSaveTeam() throws Exception {
        Team expectedTeam = new Team(1L, "Team1");

        BDDMockito.given(teamService.saveTeam(expectedTeam)).willReturn(expectedTeam);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(TeamEndpoints.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedTeam));

        mockMvc.perform(request)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(MockMvcResultMatchers.content().json(objectMapper.writeValueAsString(expectedTeam)));

        Mockito.verify(teamService, Mockito.times(1)).saveTeam(expectedTeam);
    }

    @Test
    public void testDeleteTeam() throws Exception {
        BDDMockito.given(teamService.deleteTeam(1L)).willReturn(true);

        mockMvc.perform(MockMvcRequestBuilders.delete(String.format("%s/%d", TeamEndpoints.ROOT, 1L)))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().string(Boolean.TRUE.toString()));

        Mockito.verify(teamService, Mockito.times(1)).deleteTeam(1L);
    }

    @Test
    public void testDeleteTeamNotFound() throws Exception {
        BDDMockito.given(teamService.deleteTeam(1L)).willReturn(false);

        mockMvc.perform(MockMvcRequestBuilders.delete(String.format("%s/%d", TeamEndpoints.ROOT, 1L)))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().string(Boolean.FALSE.toString()));

        Mockito.verify(teamService, Mockito.times(1)).deleteTeam(1L);
    }
}