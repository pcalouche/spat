package com.pcalouche.spat.config;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest(classes = ModelMapperConfig.class)
public class ModelMapperConfigTest {
    @Autowired
    private ModelMapper modelMapper;

    @Test
    public void testModelMapperValidation() {
        modelMapper.validate();
    }
}
