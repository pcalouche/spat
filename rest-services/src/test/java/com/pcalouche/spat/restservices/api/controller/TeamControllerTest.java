package com.pcalouche.spat.restservices.api.controller;

import com.pcalouche.spat.restservices.AbstractControllerTest;
import com.pcalouche.spat.restservices.api.ApiEndpoints;
import com.pcalouche.spat.restservices.api.EndpointMessages;
import com.pcalouche.spat.restservices.api.dto.TeamDto;
import com.pcalouche.spat.restservices.api.dto.TeamEditRequest;
import com.pcalouche.spat.restservices.service.TeamService;
import org.junit.Before;
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
import java.util.Optional;

import static org.hamcrest.Matchers.is;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willAnswer;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = TeamController.class)
public class TeamControllerTest extends AbstractControllerTest {
    @MockBean
    private TeamService teamService;
    private TeamDto testTeamDto1;
    private TeamDto testTeamDto2;

    @Before
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

        mockMvc.perform(get(ApiEndpoints.TEAMS)
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

        MockHttpServletRequestBuilder request = post(ApiEndpoints.TEAMS)
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

        MockHttpServletRequestBuilder request = post(ApiEndpoints.TEAMS)
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

        MockHttpServletRequestBuilder request = put(ApiEndpoints.TEAMS + "/" + updatedTeamDto.getId())
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(teamEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(updatedTeamDto)));

        verify(teamService, Mockito.times(1)).update(updatedTeamDto.getId(), teamEditRequest);
    }

    @Test
    public void testUpdateWhenTeamNotFound() throws Exception {
        TeamEditRequest teamEditRequest = TeamEditRequest.builder()
                .name("Team1 Change")
                .build();

        given(teamService.findById(2)).willReturn(Optional.empty());

        MockHttpServletRequestBuilder request = put(ApiEndpoints.TEAMS + "/1")
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

        MockHttpServletRequestBuilder request = put(ApiEndpoints.TEAMS + "/1")
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(teamEditRequest));

        mockMvc.perform(request)
                .andExpect(status().isForbidden());
    }

    @Test
    public void testDelete() throws Exception {
        given(teamService.findById(testTeamDto1.getId())).willReturn(Optional.of(testTeamDto1));
        willAnswer((Answer<Void>) invocationOnMock -> null).given(teamService).delete(testTeamDto1.getId());

        mockMvc.perform(delete(String.format("%s/%d", ApiEndpoints.TEAMS, testTeamDto1.getId()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isOk())
                .andExpect(content().string(""));

        verify(teamService, Mockito.times(1)).delete(testTeamDto1.getId());
    }

    @Test
    public void testDeleteWhenTeamNotFound() throws Exception {
        given(teamService.findById(testTeamDto1.getId())).willReturn(Optional.empty());
        willAnswer((Answer<Void>) invocationOnMock -> null).given(teamService).delete(testTeamDto1.getId());

        mockMvc.perform(delete(String.format("%s/%d", ApiEndpoints.TEAMS, testTeamDto1.getId()))
                .header(HttpHeaders.AUTHORIZATION, getValidAdminToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message", is(String.format(EndpointMessages.NO_TEAM_FOUND, testTeamDto1.getId()))));

        verify(teamService, Mockito.times(0)).delete(testTeamDto1.getId());
    }

    @Test
    public void testDeleteRequiresAdminRole() throws Exception {
        mockMvc.perform(delete(String.format("%s/%d", ApiEndpoints.TEAMS, 1))
                .header(HttpHeaders.AUTHORIZATION, getValidUserToken()))
                .andExpect(status().isForbidden());

        verify(teamService, Mockito.times(0)).delete(1);
    }
}