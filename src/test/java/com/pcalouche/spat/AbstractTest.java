package com.pcalouche.spat;

import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
public abstract class AbstractTest {
    // Logger instance that all child classes can use
    protected Logger logger = LoggerFactory.getLogger(getClass());
}
