package com.pcalouche.spat.restservices;

import com.pcalouche.spat.restservices.config.ModelMapperConfig;
import com.pcalouche.spat.shared.AbstractTest;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = ModelMapperConfig.class)
public abstract class AbstractServiceTest extends AbstractTest {
    @Autowired
    protected ModelMapper modelMapper;
}
