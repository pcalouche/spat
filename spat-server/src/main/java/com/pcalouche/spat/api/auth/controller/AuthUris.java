package com.pcalouche.spat.api.auth.controller;

import com.pcalouche.spat.api.BaseUris;

/**
 * Class to help manage any {@link AuthController} URIs.
 * Helpful when wanting to easily refactor URIs in large project
 */
public class AuthUris {
    public static final String ROOT = BaseUris.API_ROOT + "/auth";
    public static final String TOKEN = "token";
    public static final String REFRESH_TOKEN = "refresh-token";
}
