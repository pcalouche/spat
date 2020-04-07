package com.pcalouche.spat;

import com.pcalouche.spat.config.ModelMapperConfig;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = ModelMapperConfig.class)
public abstract class AbstractModelMapperTest extends AbstractTest {
    @Autowired
    protected ModelMapper modelMapper;
}
