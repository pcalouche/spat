package com.pcalouche.spat.dao.authorization;

import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

@Repository
public class AuthorizationDaoImpl implements AuthorizationDao {
    @Override
    public boolean isAuthorized(String authToken) {
        // Ideally this would be some real check to see if the user has access
        return !StringUtils.isEmpty(authToken) && authToken.equals("myAuthCode");
    }
}
