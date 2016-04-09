package com.calouche.spat.dao.team;

import com.calouche.spat.model.Team;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcDaoSupport;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import javax.sql.DataSource;
import java.util.List;

@Repository
public class TeamDaoImpl extends NamedParameterJdbcDaoSupport implements TeamDao {
    private static final Logger logger = LoggerFactory.getLogger(TeamDaoImpl.class);

    @Autowired
    public TeamDaoImpl(DataSource dataSource) {
        setDataSource(dataSource);
    }

    @Override
    public List<Team> getTeams() {
        return getJdbcTemplate().query(TeamQueries.GET_TEAMS, new BeanPropertyRowMapper(Team.class));
    }

    @Override
    public Team saveTeam(Team team) {
        String sql;
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource("name", team.getName())
                .addValue("sport", team.getSport())
                .addValue("league", team.getLeague());
        KeyHolder keyHolder = new GeneratedKeyHolder();

        if (team.getId() == null) {
            logger.debug("in create case");
            sql = TeamQueries.INSERT_TEAM;
        } else {
            logger.debug("in update case");
            sql = TeamQueries.UPDATE_TEAM;
            mapSqlParameterSource.addValue("id", team.getId());
        }

        getNamedParameterJdbcTemplate().update(sql, mapSqlParameterSource, keyHolder, new String[]{"id"});
        mapSqlParameterSource = new MapSqlParameterSource("id", keyHolder.getKey().longValue());
        return (Team) getNamedParameterJdbcTemplate().queryForObject(TeamQueries.GET_BY_ID, mapSqlParameterSource, new BeanPropertyRowMapper(Team.class));
    }

    @Override
    public Boolean deleteTeam(Long id) {
        logger.debug("id to delete is " + id);
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource("id", id);
        getNamedParameterJdbcTemplate().update(TeamQueries.DELETE_TEAM, mapSqlParameterSource);
        return true;
    }
}
