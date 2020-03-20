package com.pcalouche.spat.restservices;

import com.pcalouche.spat.restservices.config.ModelMapperConfig;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;

@Import({ModelMapperConfig.class})
@DataJpaTest
public abstract class AbstractServiceTest extends AbstractTest {
    @Autowired
    protected ModelMapper modelMapper;
}
