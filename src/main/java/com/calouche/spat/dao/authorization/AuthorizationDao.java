package com.calouche.spat.dao.authorization;

public interface AuthorizationDao {
    boolean isAuthorized(String authToken);
}
