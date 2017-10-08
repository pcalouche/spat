package com.pcalouche.spat.dao.user;

import com.pcalouche.spat.model.User;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.SqlParameter;
import org.springframework.jdbc.core.SqlReturnResultSet;
import org.springframework.jdbc.object.StoredProcedure;

import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GetUsersStoredProcedure extends StoredProcedure {
    private static final String NAME = "get_users";

    public GetUsersStoredProcedure(JdbcTemplate jdbcTemplate) {
        super(jdbcTemplate, NAME);
        declareParameter(new SqlParameter("val", Types.NUMERIC));
        declareParameter(new SqlReturnResultSet("result_set", new BeanPropertyRowMapper<>(User.class)));
    }


    public List<User> execute2(int val) {
        Map<String, Object> inParameters = new HashMap<>();
        inParameters.put("val", val);
        List list = (List) super.execute(inParameters).get("result_set");
        List<User> users = new ArrayList<>();
        for (Object aList : list) {
            users.add((User) aList);
        }
        return users;
    }
}
