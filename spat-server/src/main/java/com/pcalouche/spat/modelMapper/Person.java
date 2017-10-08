package com.pcalouche.spat.modelMapper;

public class Person implements AppEntity<Person, PersonDto> {
    @Override
    public PersonDto convertToDto(Person person) {
        return null;
    }
}
