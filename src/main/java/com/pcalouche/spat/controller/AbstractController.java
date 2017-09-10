package com.pcalouche.spat.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractController {
    // Logger instance that all child classes can use
    protected Logger logger = LoggerFactory.getLogger(getClass());
}
