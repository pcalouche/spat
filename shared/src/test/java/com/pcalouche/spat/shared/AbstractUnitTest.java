package com.pcalouche.spat.shared;

import org.junit.experimental.categories.Category;

/**
 * Parent class for all Unit Tests.  The JUnit Test Category is also set
 * here, so it can be leveraged by the Maven Surefire plugin configuration.
 */
@Category(UnitTestCategory.class)
public abstract class AbstractUnitTest extends AbstractTest {
}
