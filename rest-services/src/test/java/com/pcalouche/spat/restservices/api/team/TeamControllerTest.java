package com.pcalouche.spat.restservices.api.team;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.team.controller.TeamController;
import com.pcalouche.spat.restservices.api.team.controller.TeamEndpoints;
import com.pcalouche.spat.restservices.api.team.service.TeamService;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willAnswer;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = TeamController.class)
public class TeamControllerTest extends AbstractControllerTest {
    @MockBean
    private TeamService teamService;

    @Test
    public void testFindAll() throws Exception {
        List<TeamDto> expectedTeamDtos = new ArrayList<>();
        expectedTeamDtos.add(new TeamDto(1L, "Team1"));
        expectedTeamDtos.add(new TeamDto(2L, "Team2"));

        given(teamService.findAll()).willReturn(expectedTeamDtos);

        mockMvc.perform(get(TeamEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(expectedTeamDtos)));

        verify(teamService, Mockito.times(1)).findAll();
    }

    @Test
    public void testSave() throws Exception {
        TeamDto expectedTeamDto = new TeamDto(1L, "Team1");

        given(teamService.save(expectedTeamDto)).willReturn(expectedTeamDto);

        MockHttpServletRequestBuilder request = post(TeamEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedTeamDto));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(expectedTeamDto)));

        verify(teamService, Mockito.times(1)).save(expectedTeamDto);
    }

    @Test
    public void testSaveRequiresAdminRole() throws Exception {
        TeamDto expectedTeamDto = new TeamDto(1L, "Team1");

        given(teamService.save(expectedTeamDto)).willReturn(expectedTeamDto);

        MockHttpServletRequestBuilder request = post(TeamEndpoints.ROOT)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(expectedTeamDto));

        mockMvc.perform(request)
                .andExpect(status().isForbidden())
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON_UTF8));
    }

    @Test
    public void testDelete() throws Exception {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(teamService).deleteById(1L);

        mockMvc.perform(delete(String.format("%s/%d", TeamEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(teamService, Mockito.times(1)).deleteById(1L);
    }

    @Test
    public void testDeleteByIdNotFound() throws Exception {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(teamService).deleteById(1L);

        mockMvc.perform(delete(String.format("%s/%d", TeamEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(teamService, Mockito.times(1)).deleteById(1L);
    }

    @Test
    public void testDeleteByIdRequiresAdminRole() throws Exception {
        willAnswer((Answer<Void>) invocationOnMock -> null).given(teamService).deleteById(1L);

        mockMvc.perform(delete(String.format("%s/%d", TeamEndpoints.ROOT, 1L))
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isForbidden())
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON_UTF8));
    }
}