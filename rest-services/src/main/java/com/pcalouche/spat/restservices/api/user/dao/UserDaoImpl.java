package com.pcalouche.spat.restservices.api.user.dao;

import com.pcalouche.spat.restservices.api.AbstractSpatDaoImpl;
import com.pcalouche.spat.restservices.api.entity.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Repository;

import javax.sql.DataSource;
import java.util.List;
import java.util.stream.Collectors;

@Repository
public class UserDaoImpl extends AbstractSpatDaoImpl implements UserDao {
    private final PasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    @Autowired
    public UserDaoImpl(@Qualifier("dataSource") DataSource dataSource) {
        setDataSource(dataSource);
    }

    @Override
    public User getByUsername(String username) {
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource()
                .addValue("username", username);
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
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource()
                .addValue("username", user.getUsername())
                .addValue("password", passwordEncoder.encode(user.getPassword()))
                .addValue("accountNonExpired", user.isAccountNonExpired())
                .addValue("accountNonLocked", user.isAccountNonLocked())
                .addValue("credentialsNonExpired", user.isCredentialsNonExpired())
                .addValue("enabled", user.isEnabled())
                .addValue("authorities", user.getAuthorities()
                        .stream()
                        .map(GrantedAuthority::getAuthority)
                        .collect(Collectors.joining(",")));

        if (user.getId() == null) {
            logger.debug("in create case");
            sql = UserQueries.INSERT_USER;
            KeyHolder keyHolder = new GeneratedKeyHolder();
            getNamedParameterJdbcTemplate().update(sql, mapSqlParameterSource, keyHolder, new String[]{"id"});
            mapSqlParameterSource.addValue("id", keyHolder.getKey().longValue());
        } else {
            logger.debug("in update case");
            sql = UserQueries.UPDATE_USER;
            // Query too see if it exists in the database first
            mapSqlParameterSource.addValue("id", user.getId());
            getNamedParameterJdbcTemplate().queryForObject(UserQueries.GET_BY_ID, mapSqlParameterSource, new BeanPropertyRowMapper<>(User.class));
            getNamedParameterJdbcTemplate().update(sql, mapSqlParameterSource);
        }

        return getNamedParameterJdbcTemplate().queryForObject(UserQueries.GET_BY_ID, mapSqlParameterSource, new BeanPropertyRowMapper<>(User.class));
    }

    @Override
    public Boolean deleteUser(Long id) {
        logger.debug("id to delete is " + id);
        MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource()
                .addValue("id", id);
        int numRowsAffected = getNamedParameterJdbcTemplate().update(UserQueries.DELETE_USER, mapSqlParameterSource);
        return numRowsAffected > 0;
    }
}
