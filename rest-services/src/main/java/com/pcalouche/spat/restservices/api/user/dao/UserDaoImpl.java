package com.pcalouche.spat.restservices.api.user.dao;

import com.pcalouche.spat.restservices.api.AbstractSpatDaoImpl;
import com.pcalouche.spat.restservices.api.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import javax.sql.DataSource;
import java.util.List;

@Repository
public class UserDaoImpl extends AbstractSpatDaoImpl implements UserDao {
    @Autowired
    public UserDaoImpl(@Qualifier("dataSource") DataSource dataSource) {
        setDataSource(dataSource);
    }

    @Override
    public User getByUsername(String username) {
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource("username", username);
        try {
            return getNamedParameterJdbcTemplate().queryForObject(UserQueries.GET_BY_USERNAME, mapSqlParameterSource, new BeanPropertyRowMapper<>(User.class));
        } catch (EmptyResultDataAccessException e) {
            return null;
        }
    }

    @Override
    public List<User> getUsers() {
        return getJdbcTemplate().query(UserQueries.GET_USERS, new BeanPropertyRowMapper<>(User.class));
    }

    @Override
    public User saveUser(User user) {
        String sql;
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource("firstName", user.getFirstName())
                .addValue("lastName", user.getLastName());
        KeyHolder keyHolder = new GeneratedKeyHolder();

        if (user.getId() == null) {
            logger.debug("in create case");
            sql = UserQueries.INSERT_USER;
        } else {
            logger.debug("in update case");
            // Query too see if it exists in the database first
            mapSqlParameterSource.addValue("id", user.getId());
            getNamedParameterJdbcTemplate().queryForObject(UserQueries.GET_BY_ID, mapSqlParameterSource, new BeanPropertyRowMapper<>(User.class));
            sql = UserQueries.UPDATE_USER;
        }

        getNamedParameterJdbcTemplate().update(sql, mapSqlParameterSource, keyHolder, new String[]{"id"});
        mapSqlParameterSource = new MapSqlParameterSource("id", keyHolder.getKey().longValue());
        return getNamedParameterJdbcTemplate().queryForObject(UserQueries.GET_BY_ID, mapSqlParameterSource, new BeanPropertyRowMapper<>(User.class));
    }

    @Override
    public Boolean deleteUser(Long id) {
        logger.debug("id to delete is " + id);
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource("id", id);
        int numRowsAffected = getNamedParameterJdbcTemplate().update(UserQueries.DELETE_USER, mapSqlParameterSource);
        return numRowsAffected > 0;
    }
}
