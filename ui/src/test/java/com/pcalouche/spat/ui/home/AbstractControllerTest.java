package com.pcalouche.spat.ui.home;

import com.pcalouche.spat.shared.AbstractUnitTest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.servlet.MockMvc;

public abstract class AbstractControllerTest extends AbstractUnitTest {
    @Autowired
    protected MockMvc mockMvc;
}
