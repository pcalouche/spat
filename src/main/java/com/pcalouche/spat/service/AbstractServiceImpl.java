package com.pcalouche.spat.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AbstractServiceImpl {
    // Logger instance that all child classes can use
    protected Logger logger = LoggerFactory.getLogger(getClass());
}
