package com.pcalouche.spat;

import com.pcalouche.spat.config.ModelMapperConfig;
import org.junit.jupiter.api.extension.ExtendWith;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@Import({ModelMapperConfig.class})
@DataJpaTest
public abstract class AbstractServiceTest {
    @Autowired
    protected ModelMapper modelMapper;
}
