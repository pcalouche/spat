package com.pcalouche.spat.restservices.api.entity;

import org.modelmapper.ModelMapper;

public interface DtoConvertible<DTO> {
    DTO convertToDto(ModelMapper modelMapper);
}
