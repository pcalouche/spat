package com.pcalouche.spat.dao.authorization;

public interface AuthorizationDao {
    boolean isAuthorized(String authToken);
}
