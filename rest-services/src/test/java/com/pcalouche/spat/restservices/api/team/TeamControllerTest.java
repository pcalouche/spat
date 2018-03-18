package com.pcalouche.spat.restservices.api.team;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.entity.Team;
import com.pcalouche.spat.restservices.api.team.controller.TeamController;
import com.pcalouche.spat.restservices.api.team.controller.TeamEndpoints;
import com.pcalouche.spat.restservices.api.team.service.TeamService;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.BDDMockito.given;
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

        List<TeamDto> expectedTeamDtos = new ArrayList<>();
        expectedTeamDtos.add(new TeamDto(1L, "Team1"));
        expectedTeamDtos.add(new TeamDto(2L, "Team2"));

        given(teamService.getTeams()).willReturn(expectedTeams);

        mockMvc.perform(get(TeamEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(expectedTeamDtos)));

        verify(teamService, Mockito.times(1)).getTeams();
    }

    @Test
    public void testSaveTeam() throws Exception {
        Team expectedTeam = new Team(1L, "Team1");
        TeamDto expectedTeamDto = new TeamDto(1L, "Team1");

        given(teamService.saveTeam(expectedTeam)).willReturn(expectedTeam);

        MockHttpServletRequestBuilder request = post(TeamEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedTeamDto));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(expectedTeamDto)));

        verify(teamService, Mockito.times(1)).saveTeam(expectedTeam);
    }

    @Test
    public void testSaveTeamRequiresAdminRole() throws Exception {
        Team expectedTeam = new Team(1L, "Team1");
        TeamDto expectedTeamDto = new TeamDto(1L, "Team1");

        given(teamService.saveTeam(expectedTeam)).willReturn(expectedTeam);

        MockHttpServletRequestBuilder request = post(TeamEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedTeamDto));

        mockMvc.perform(request)
                .andExpect(status().isForbidden())
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON_UTF8));
    }

    @Test
    public void testDeleteTeam() throws Exception {
        given(teamService.deleteTeam(1L)).willReturn(true);

        mockMvc.perform(delete(String.format("%s/%d", TeamEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(Boolean.TRUE.toString()));

        verify(teamService, Mockito.times(1)).deleteTeam(1L);
    }

    @Test
    public void testDeleteTeamNotFound() throws Exception {
        given(teamService.deleteTeam(1L)).willReturn(false);

        mockMvc.perform(delete(String.format("%s/%d", TeamEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(Boolean.FALSE.toString()));

        verify(teamService, Mockito.times(1)).deleteTeam(1L);
    }

    @Test
    public void testDeleteTeamRequireAdminRole() throws Exception {
        given(teamService.deleteTeam(1L)).willReturn(true);

        mockMvc.perform(delete(String.format("%s/%d", TeamEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isForbidden())
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON_UTF8));
    }
}