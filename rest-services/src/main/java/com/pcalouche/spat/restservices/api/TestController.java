package com.pcalouche.spat.restservices.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/api/test")
public class TestController {

    @PostMapping(value = "/json-node")
    public void parseJsonNode(@RequestBody JsonNode jsonNode) {
        if (jsonNode.get("y") != null) {
            System.out.println(jsonNode.get("y"));
            System.out.println(jsonNode.get("y").getNodeType());
        }
    }

    @PostMapping(value = "/object-node")
    public void parseObjectNode(@RequestBody ObjectNode objectNode) {
        if (objectNode.get("y") != null) {
            System.out.println(objectNode.get("y"));
            System.out.println(objectNode.get("y").getNodeType());
        }
        objectNode.put("u", 42);
        System.out.println(objectNode.get("u"));
        System.out.println(objectNode.get("u").getNodeType());
    }
}
