package com.pcalouche.spat.restservices;

import com.pcalouche.spat.restservices.config.ModelMapperConfig;
import com.pcalouche.spat.shared.AbstractUnitTest;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = ModelMapperConfig.class)
public abstract class AbstractModelMapperTest extends AbstractUnitTest {
    @Autowired
    protected ModelMapper modelMapper;
}
