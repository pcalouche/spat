package com.pcalouche.spat.restservices.api.dto;

import org.modelmapper.ModelMapper;

public interface EntityConvertible<ENTITY> {
    ENTITY convertToEntity(ModelMapper modelMapper);
}
