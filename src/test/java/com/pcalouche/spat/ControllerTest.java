package com.pcalouche.spat;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pcalouche.spat.interceptors.AuthorizationInterceptor;
import com.pcalouche.spat.interceptors.LoggerInterceptor;
import org.junit.Before;
import org.junit.experimental.categories.Category;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;

@Category(UnitTestCategory.class)
public abstract class ControllerTest extends AbstractTest {
    protected final ObjectMapper objectMapper = new ObjectMapper();
    @MockBean
    protected LoggerInterceptor loggerInterceptor;
    @MockBean
    protected AuthorizationInterceptor authorizationInterceptor;
    @Autowired
    protected MockMvc mockMvc;

    @Before
    public void setup() throws Exception {
        given(loggerInterceptor.preHandle(any(), any(), any())).willReturn(true);
        given(authorizationInterceptor.preHandle(any(), any(), any())).willReturn(true);
    }
}
