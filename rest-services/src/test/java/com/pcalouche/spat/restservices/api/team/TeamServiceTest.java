package com.pcalouche.spat.restservices.api.team;

import com.pcalouche.spat.restservices.AbstractServiceTest;

public class TeamServiceTest extends AbstractServiceTest {
    //    @MockBean
    //    private TeamDao teamDao;
    //    @MockBean
    //    private TeamRepository teamRepository;
    //    private TeamService teamService;
    //
    //    @Before
    //    public void before() {
    //        teamService = new TeamServiceImpl(teamDao, teamRepository);
    //    }
    //
    //    @Test
    //    public void testGetTeams() {
    //        List<Team> expectedTeams = new ArrayList<>();
    //        expectedTeams.add(new Team(1L, "Team1"));
    //        expectedTeams.add(new Team(2L, "Team2"));
    //
    //        given(teamDao.getTeams()).willReturn(expectedTeams);
    //
    //        assertThat(teamService.getTeams()).isEqualTo(expectedTeams);
    //
    //        verify(teamDao, Mockito.times(1)).getTeams();
    //    }
    //
    //    @Test
    //    public void testSaveTeam() {
    //        Team expectedTeam = new Team(1L, "Team1");
    //
    //        given(teamDao.saveTeam(expectedTeam)).willReturn(expectedTeam);
    //
    //        assertThat(teamService.saveTeam(expectedTeam)).isEqualTo(expectedTeam);
    //
    //        verify(teamDao, Mockito.times(1)).saveTeam(expectedTeam);
    //    }
    //
    //    @Test
    //    public void testDeleteTeam() {
    //        given(teamDao.deleteTeam(1L)).willReturn(true);
    //
    //        assertThat(teamService.deleteTeam(1L)).isTrue();
    //    }
}
