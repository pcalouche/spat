package com.pcalouche.spat.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class ExceptionUtils {
    private static final ObjectMapper objectMapper = new ObjectMapper();

    public static JsonNode buildJsonErrorObject(Exception e) {
        ObjectNode errorObjectNode = objectMapper.createObjectNode();
        errorObjectNode.put("type", e.getClass().getName());
        errorObjectNode.put("message", e.getMessage());
        return objectMapper.createObjectNode().set("error", errorObjectNode);
    }
}
