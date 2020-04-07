package com.pcalouche.spat.config;

import com.pcalouche.spat.AbstractModelMapperTest;
import org.junit.jupiter.api.Test;

public class ModelMapperConfigTest extends AbstractModelMapperTest {
    /**
     * Test if we have any unmapped fields that were not explicitly
     * called out as skipped.
     */
    @Test
    public void testModelMapperValidation() {
        modelMapper.validate();
    }
}
