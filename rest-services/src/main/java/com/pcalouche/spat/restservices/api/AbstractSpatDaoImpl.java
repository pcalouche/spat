package com.pcalouche.spat.restservices.api;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcDaoSupport;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public abstract class AbstractSpatDaoImpl extends NamedParameterJdbcDaoSupport {
    protected final Logger logger = LoggerFactory.getLogger(getClass());
}
