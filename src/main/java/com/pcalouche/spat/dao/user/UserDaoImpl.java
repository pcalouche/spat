package com.pcalouche.spat.dao.user;

import com.pcalouche.spat.model.User;
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
public class UserDaoImpl extends NamedParameterJdbcDaoSupport implements UserDao {
    private static final Logger logger = LoggerFactory.getLogger(UserDao.class);

    @Autowired
    public UserDaoImpl(DataSource dataSource) {
        setDataSource(dataSource);
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
            sql = UserQueries.UPDATE_USER;
            mapSqlParameterSource.addValue("id", user.getId());
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