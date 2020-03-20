package com.pcalouche.spat.restservices;

import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * Parent class for all tests and for other abstract test classes.
 * The test runner type and property configuration for all tests
 * are set here.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource({"classpath:application-test.properties"})
public abstract class AbstractTest {
    // Logger instance that all child classes can use
    protected final Logger logger = LoggerFactory.getLogger(getClass());
}