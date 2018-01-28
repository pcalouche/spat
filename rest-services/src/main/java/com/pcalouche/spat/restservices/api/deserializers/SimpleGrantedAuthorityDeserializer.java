package com.pcalouche.spat.restservices.api.deserializers;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class SimpleGrantedAuthorityDeserializer extends JsonDeserializer<List<SimpleGrantedAuthority>> {
    @Override
    public List<SimpleGrantedAuthority> deserialize(JsonParser p, DeserializationContext ctxt) throws IOException, JsonProcessingException {
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        JsonNode jsonNode = p.getCodec().readTree(p);
        if (jsonNode.isArray()) {
            for (JsonNode child : jsonNode) {
                authorities.add(new SimpleGrantedAuthority(child.get("authority").asText()));
            }
        }
        return authorities;
    }
}
