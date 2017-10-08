package com.pcalouche.spat.modelMapper;

public class PersonDto implements AppDto<PersonDto, Person> {
    @Override
    public Person convertToEntity(PersonDto personDto) {
        return null;
    }
}
