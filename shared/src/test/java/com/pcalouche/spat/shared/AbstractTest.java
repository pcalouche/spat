package com.pcalouche.spat.shared;

import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Parent class for all tests and for other abstract test classes.
 * See {@link AbstractUnitTest} and {@link AbstractIntegrationTest}.
 * The test runner type and property configuration for all tests
 * are set here.
 */
@RunWith(SpringRunner.class)
@TestPropertySource({"classpath:application-test.properties"})
public abstract class AbstractTest {
    // Logger instance that all child classes can use
    protected final Logger logger = LoggerFactory.getLogger(getClass());
}