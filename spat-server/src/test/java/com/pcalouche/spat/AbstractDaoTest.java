package com.pcalouche.spat;

import org.junit.experimental.categories.Category;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.transaction.annotation.Transactional;

@SpringBootTest
@Transactional
@Category(IntegrationTestCategory.class)
public abstract class AbstractDaoTest extends AbstractTest {
}
