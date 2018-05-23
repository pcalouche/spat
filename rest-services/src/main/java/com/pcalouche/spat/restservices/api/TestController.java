package com.pcalouche.spat.restservices.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Rough example to show using Jackson's JsonNode and ObjectNode to parse some
 * arbitrary JSON.  Assume your have a POST with a JSON request body as follows:
 * <pre>
 * {
 * 	"formType": "formA",
 * 	"formData": {
 * 		"field1": "field1Value",
 * 		"comments": [
 *                        {
 * 				"id": 1,
 * 				"text": "comment 1 text"
 *            },
 *            {
 * 				"id": 2,
 * 				"text": "comment 2 text"
 *            }
 * 		]
 *    }
 * }
 */
@RestController
@RequestMapping(value = "/api/test")
public class TestController {
    private final ObjectMapper objectMapper;

    public TestController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    // JsonNode is immutable.  Great if you just need to read the arbitrary JSON
    @PostMapping(value = "/json-node")
    public void parseJsonNode(@RequestBody JsonNode jsonNode) {
        // Probably could be made more elegant to check if keys exists or not
        if (jsonNode.get("formType") != null && jsonNode.get("formData") != null) {
            System.out.println(jsonNode.get("formType"));
            System.out.println(jsonNode.get("formData"));
            System.out.println(jsonNode.get("formData").getNodeType()); // Prints Object
        }
    }

    // Use ObjectNode if you need to manipulate the JSON
    @PostMapping(value = "/object-node")
    public void parseObjectNode(@RequestBody ObjectNode objectNode) {
        if (objectNode.get("formType") != null && objectNode.get("formData") != null) {
            System.out.println(objectNode.get("formType"));
            System.out.println(objectNode.get("formData"));
            System.out.println(objectNode.get("formData").getNodeType());
        }
        // Adds a new comment to the array
        ObjectNode commentNode = objectMapper.createObjectNode();
        commentNode.put("id", 3);
        commentNode.put("text", "comment 3 text");
        ((ArrayNode) objectNode.get("formData").get("comments")).add(commentNode);

        System.out.println(objectNode.get("formData").get("comments"));
    }
}
