package com.calouche.spat;

import com.calouche.spat.config.SpatWebMvcConfigurerAdapter;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.testng.AbstractTransactionalTestNGSpringContextTests;
import org.springframework.test.context.web.WebAppConfiguration;

@ContextConfiguration(classes = {
        SpatWebMvcConfigurerAdapter.class
})
@WebAppConfiguration
public class IntegratedTest extends AbstractTransactionalTestNGSpringContextTests {
}
