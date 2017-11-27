package com.pcalouche.spat.api;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractController {
    public static final String API_ROOT = "/api";
    // Logger instance that all child classes can use
    protected final Logger logger = LoggerFactory.getLogger(getClass());
}
