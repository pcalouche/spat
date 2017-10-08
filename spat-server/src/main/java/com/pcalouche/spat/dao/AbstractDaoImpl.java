package com.pcalouche.spat.dao;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcDaoSupport;

public abstract class AbstractDaoImpl extends NamedParameterJdbcDaoSupport {
    // Logger instance that all child classes can use
    protected final Logger logger = LoggerFactory.getLogger(getClass());
}
