package com.calouche.spat.dao.authorization;

import org.springframework.stereotype.Repository;

@Repository
public class AuthorizationDaoImpl implements AuthorizationDao {
    @Override
    public boolean isAuthorized(String authToken) {
        // Ideally this would be some real check to see if the user has access
        return authToken.equals("myAuthCode");
    }
}
