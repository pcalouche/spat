package com.pcalouche.spat;

import org.springframework.boot.test.mock.mockito.MockBean;

public abstract class AbstractTest2<ENTITY, SERVICE> {
    @MockBean
    protected SERVICE service;
    @MockBean
    protected ENTITY entity;
}
