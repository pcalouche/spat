package com.pcalouche.spat.dao;

import com.pcalouche.spat.AbstractDaoTest;
import com.pcalouche.spat.dao.team.TeamDao;
import com.pcalouche.spat.model.Team;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

public class TeamDaoTest extends AbstractDaoTest {
    @Autowired
    private TeamDao teamDao;

    @Test
    public void testGetTeams() {
        List<Team> teams = teamDao.getTeams();
        assertThat(teams).isNotNull();
    }

    @Test
    public void testSaveTeam() {
        Team newTeam = new Team(null, "Team1");

        Team savedTeam = teamDao.saveTeam(newTeam);
        assertThat(savedTeam).isEqualTo(newTeam);

        savedTeam.setName("NewName");

        Team updatedTeam = teamDao.saveTeam(savedTeam);
        assertThat(updatedTeam).isEqualTo(savedTeam);
    }

    @Test
    public void testDeleteTeam() {
        assertThat(teamDao.deleteTeam(1L)).isTrue();
        assertThat(teamDao.deleteTeam(-1L)).isFalse();
    }
}
