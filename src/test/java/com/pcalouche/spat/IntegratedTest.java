package com.pcalouche.spat;

import com.pcalouche.spat.config.SpatTestConfig;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.testng.AbstractTransactionalTestNGSpringContextTests;
import org.springframework.test.context.web.WebAppConfiguration;

@ContextConfiguration(classes = {
        SpatTestConfig.class
})
@WebAppConfiguration
public class IntegratedTest extends AbstractTransactionalTestNGSpringContextTests {
}
