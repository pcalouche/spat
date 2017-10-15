package com.pcalouche.spat;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.interceptors.LoggerInterceptor;
import org.junit.Before;
import org.junit.experimental.categories.Category;
import org.mockito.BDDMockito;
import org.mockito.Matchers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

@Category(UnitTestCategory.class)
public abstract class AbstractControllerTest extends AbstractTest {
    protected final ObjectMapper objectMapper = new ObjectMapper();
    @MockBean
    protected LoggerInterceptor loggerInterceptor;
    @Autowired
    protected MockMvc mockMvc;

    @Before
    public void setup() throws Exception {
        BDDMockito.given(loggerInterceptor.preHandle(Matchers.any(), Matchers.any(), Matchers.any())).willReturn(true);
    }
}
