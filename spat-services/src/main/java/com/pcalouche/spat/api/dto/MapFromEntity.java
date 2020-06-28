package com.pcalouche.spat.api.dto;

@FunctionalInterface
public interface MapFromEntity<T, E> {
    T mapFromEntity(E e);
}
