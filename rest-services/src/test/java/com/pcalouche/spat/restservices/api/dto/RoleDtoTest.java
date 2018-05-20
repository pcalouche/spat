package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.entity.Role;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class RoleDtoTest extends AbstractModelMapperTest {

    @Test
    public void testRoleModelMapper() {
        Role role = new Role();
        role.setId(1L);
        role.setName("ROLE_USER");

        RoleDto roleDto = modelMapper.map(role, RoleDto.class);
        assertThat(roleDto.getId()).isEqualTo(1L);
        assertThat(roleDto.getName()).isEqualTo("ROLE_USER");
    }
}
