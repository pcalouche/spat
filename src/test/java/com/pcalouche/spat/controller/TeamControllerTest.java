package com.pcalouche.spat.controller;

import com.pcalouche.spat.controller.team.TeamController;
import com.pcalouche.spat.controller.team.TeamControllerUris;
import com.pcalouche.spat.model.Team;
import com.pcalouche.spat.service.team.TeamService;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class TeamControllerTest {
    private final TeamService teamService = Mockito.mock(TeamService.class);
    private final TeamController teamController = new TeamController(teamService);
    private final MockMvc mockMvc = MockMvcBuilders.standaloneSetup(teamController).build();
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    public void getTeamsTest() throws Exception {
        List<Team> expectedTeams = new ArrayList<>();
        expectedTeams.add(new Team(1L, "Team1", "Basketball", "NBA"));
        expectedTeams.add(new Team(2L, "Team2", "Soccer", "FIBA"));

        Mockito.when(teamService.getTeams()).thenReturn(expectedTeams);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(TeamControllerUris.ROOT);
        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        List<Team> actualTeams = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<List<Team>>() {
        });
        Assert.assertEquals(actualTeams.size(), expectedTeams.size());
        for (int i = 0; i < actualTeams.size(); i++) {
            Assert.assertEquals(actualTeams.get(i), expectedTeams.get(i));
        }
    }

    @Test
    public void saveTeamTest() throws Exception {
        Team expectedTeam = new Team(1L, "Team1", "Basketball", "NBA");

        Mockito.when(teamService.saveTeam(expectedTeam)).thenReturn(expectedTeam);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(TeamControllerUris.ROOT)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedTeam));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        Team actualTeam = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), Team.class);
        Assert.assertEquals(actualTeam, expectedTeam);
    }

    @Test
    public void deleteTeamTest() throws Exception {
        Mockito.when(teamService.deleteTeam(1L)).thenReturn(true);

        MockHttpServletRequestBuilder request = MockMvcRequestBuilders.delete(String.format("%s/%d", TeamControllerUris.ROOT, 1L));

        MvcResult mvcResult = mockMvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();

        Assert.assertTrue(Boolean.valueOf(mvcResult.getResponse().getContentAsString()));
    }
}