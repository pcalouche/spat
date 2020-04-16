package com.pcalouche.spat.api.controller;

import com.pcalouche.spat.AbstractControllerTest;
import com.pcalouche.spat.api.EndpointMessages;
import com.pcalouche.spat.api.Endpoints;
import com.pcalouche.spat.api.dto.TeamDto;
import com.pcalouche.spat.api.dto.TeamEditRequest;
import com.pcalouche.spat.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.hamcrest.Matchers.is;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(TeamController.class)
public class TeamControllerTest extends AbstractControllerTest {
    @MockBean
    private TeamService teamService;
    private TeamDto testTeamDto1;
    private TeamDto testTeamDto2;

    @BeforeEach
    public void before() {
        testTeamDto1 = TeamDto.builder()
                .id(1)
                .name("Team1")
                .build();
        testTeamDto2 = TeamDto.builder()
                .id(2)
                .name("Team2")
                .build();
    }

    @Test
    public void testFindAll() throws Exception {
        List<TeamDto> expectedTeamDtos = new ArrayList<>();
        expectedTeamDtos.add(testTeamDto1);
        expectedTeamDtos.add(testTeamDto2);

        given(teamService.findAll()).willReturn(expectedTeamDtos);

        mockMvc.perform(MockMvcRequestBuilders.get(Endpoints.TEAMS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(expectedTeamDtos)));

        verify(teamService, Mockito.times(1)).findAll();
    }

    @Test
    public void testCreate() throws Exception {
        TeamEditRequest teamEditRequest = TeamEditRequest.builder()
                .name("Team1")
                .build();

        given(teamService.create(teamEditRequest)).willReturn(testTeamDto1);

        MockHttpServletRequestBuilder request = post(Endpoints.TEAMS)
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(teamEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(testTeamDto1)));

        verify(teamService, Mockito.times(1)).create(teamEditRequest);
    }

    @Test
    public void testCreateRequiresAdminRole() throws Exception {
        TeamEditRequest teamEditRequest = TeamEditRequest.builder()
                .name("Team1")
                .build();

        given(teamService.create(teamEditRequest)).willReturn(testTeamDto1);

        MockHttpServletRequestBuilder request = post(Endpoints.TEAMS)
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(teamEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());

        verify(teamService, Mockito.times(0)).create(teamEditRequest);
    }

    @Test
    public void testUpdate() throws Exception {
        TeamEditRequest teamEditRequest = TeamEditRequest.builder()
                .name("Team1 Change")
                .build();

        TeamDto updatedTeamDto = TeamDto.builder()
                .id(1)
                .name(teamEditRequest.getName())
                .build();

        given(teamService.findById(testTeamDto1.getId())).willReturn(Optional.of(testTeamDto1));
        given(teamService.update(updatedTeamDto.getId(), teamEditRequest)).willReturn(Optional.of(updatedTeamDto));

        MockHttpServletRequestBuilder request = put(Endpoints.TEAMS + "/" + updatedTeamDto.getId())
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(teamEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(updatedTeamDto)));
    }

    @Test
    public void testUpdateHandlesTeamNotFound() throws Exception {
        TeamEditRequest teamEditRequest = TeamEditRequest.builder()
                .name("Team1 Change")
                .build();

        given(teamService.findById(2)).willReturn(Optional.empty());

        MockHttpServletRequestBuilder request = put(Endpoints.TEAMS + "/1")
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(teamEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_TEAM_FOUND, testTeamDto1.getId()))));
    }

    @Test
    public void testUpdateRequiresAdminRole() throws Exception {
        TeamEditRequest teamEditRequest = TeamEditRequest.builder()
                .name("Team1 Change")
                .build();

        MockHttpServletRequestBuilder request = put(Endpoints.TEAMS + "/1")
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(teamEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());
    }

    @Test
    public void testDelete() throws Exception {
        given(teamService.findById(testTeamDto1.getId())).willReturn(Optional.of(testTeamDto1));

        mockMvc.perform(delete(String.format("%s/%d", Endpoints.TEAMS, testTeamDto1.getId()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(teamService, Mockito.times(1)).delete(testTeamDto1.getId());
    }

    @Test
    public void testDeleteWhenTeamNotFound() throws Exception {
        given(teamService.findById(testTeamDto1.getId())).willReturn(Optional.empty());

        mockMvc.perform(delete(String.format("%s/%d", Endpoints.TEAMS, testTeamDto1.getId()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_TEAM_FOUND, testTeamDto1.getId()))));

        verify(teamService, Mockito.times(0)).delete(testTeamDto1.getId());
    }

    @Test
    public void testDeleteRequiresAdminRole() throws Exception {
        mockMvc.perform(delete(String.format("%s/%d", Endpoints.TEAMS, 1))
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isForbidden());
    }
}