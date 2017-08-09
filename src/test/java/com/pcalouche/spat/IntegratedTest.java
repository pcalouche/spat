package com.pcalouche.spat;

import com.pcalouche.spat.config.SpatConfig;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.testng.AbstractTransactionalTestNGSpringContextTests;
import org.springframework.test.context.web.WebAppConfiguration;

@ContextConfiguration(classes = {
        SpatConfig.class
})
@WebAppConfiguration
public class IntegratedTest extends AbstractTransactionalTestNGSpringContextTests {
}
