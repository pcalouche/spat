package com.pcalouche.spat.restservices;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.restservices.interceptors.LoggerInterceptor;
import org.junit.Before;
import org.junit.experimental.categories.Category;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;

@Category(UnitTestCategory.class)
public abstract class AbstractControllerTest extends AbstractUnitTest {
    @Autowired
    protected ObjectMapper objectMapper;
    @MockBean
    protected LoggerInterceptor loggerInterceptor;
    @Autowired
    protected MockMvc mockMvc;

    @Before
    public void setup() {
        given(loggerInterceptor.preHandle(any(), any(), any())).willReturn(true);
    }
}
