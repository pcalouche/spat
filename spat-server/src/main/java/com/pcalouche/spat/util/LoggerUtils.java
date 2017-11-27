package com.pcalouche.spat.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerUtils {
    private static final Logger logger = LoggerFactory.getLogger(LoggerUtils.class);

    public static void logException(Exception e) {
        logException(String.format("%s occurred", e.getClass().getName()), e);
    }

    public static void logException(String contextMessage, Exception e) {
        logger.error(contextMessage, e);
    }

    public static void logWarn(String msg) {
        logger.warn(msg);
    }

    public static void logInfo(String msg) {
        logger.info(msg);
    }

    public static void logDebug(String msg) {
        logger.debug(msg);
    }

    public static void logDebug(String msg, Object obj) {
        logger.debug(msg, obj);
    }

    public static void logDebug(String msg, Exception e) {
        logger.debug(msg, e);
    }
}