package com.pcalouche.spat.restservices;

import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@TestPropertySource(locations = {"classpath:/application-test.properties"})
public abstract class AbstractTest {
    // Logger instance that all child classes can use
    protected final Logger logger = LoggerFactory.getLogger(getClass());
}
