package com.pcalouche.spat.modelMapper;

public interface AppEntity<ENTITY, DTO> {
    DTO convertToDto(ENTITY entity);
}
