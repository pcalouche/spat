package com.pcalouche.spat.restservices;

import org.junit.experimental.categories.Category;
import org.springframework.boot.test.context.SpringBootTest;

/**
 * Parent class for all Integrated Tests.  These types of tests load
 * the Spring Boot context.  It is also worth noting that Transactional
 * is set here, so by default all database changes in tests are rolled.
 * back.  This behavior can be changed at the test level, but for the vast
 * majority of test this is the desired behavior when testing against an
 * actual database.  The JUnit Test Category is also set here, so it can
 * be leveraged by the Maven Failsafe plugin configuration.
 */
@SpringBootTest
@Category(IntegrationTestCategory.class)
public abstract class AbstractIntegrationTest extends AbstractTest {
}
