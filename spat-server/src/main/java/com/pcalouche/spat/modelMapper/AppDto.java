package com.pcalouche.spat.modelMapper;

public interface AppDto<DTO, ENTITY> {
    ENTITY convertToEntity(DTO dto);
}
