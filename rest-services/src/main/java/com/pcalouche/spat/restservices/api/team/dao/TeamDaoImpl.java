package com.pcalouche.spat.restservices.api.team.dao;

import com.pcalouche.spat.restservices.api.AbstractSpatDaoImpl;
import com.pcalouche.spat.restservices.api.model.Team;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import javax.sql.DataSource;
import java.util.List;

@Repository
public class TeamDaoImpl extends AbstractSpatDaoImpl implements TeamDao {
    @Autowired
    public TeamDaoImpl(@Qualifier("dataSource") DataSource dataSource) {
        setDataSource(dataSource);
    }

    @Override
    public List<Team> getTeams() {
        return getJdbcTemplate().query(TeamQueries.GET_TEAMS, new BeanPropertyRowMapper<>(Team.class));
    }

    @Override
    public Team saveTeam(Team team) {
        String sql;
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource("name", team.getName());
        KeyHolder keyHolder = new GeneratedKeyHolder();

        if (team.getId() == null) {
            logger.debug("in create case");
            sql = TeamQueries.INSERT_TEAM;
        } else {
            logger.debug("in update case");
            // Query too see if it exists in the database first
            mapSqlParameterSource.addValue("id", team.getId());
            getNamedParameterJdbcTemplate().queryForObject(TeamQueries.GET_BY_ID, mapSqlParameterSource, new BeanPropertyRowMapper<>(Team.class));
            sql = TeamQueries.UPDATE_TEAM;
        }

        getNamedParameterJdbcTemplate().update(sql, mapSqlParameterSource, keyHolder, new String[]{"id"});
        mapSqlParameterSource = new MapSqlParameterSource("id", keyHolder.getKey().longValue());
        return getNamedParameterJdbcTemplate().queryForObject(TeamQueries.GET_BY_ID, mapSqlParameterSource, new BeanPropertyRowMapper<>(Team.class));
    }

    @Override
    public Boolean deleteTeam(Long id) {
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource("id", id);
        int numRowsAffected = getNamedParameterJdbcTemplate().update(TeamQueries.DELETE_TEAM, mapSqlParameterSource);
        return numRowsAffected > 0;
    }
}
