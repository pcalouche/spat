package com.pcalouche.spat.restservices.api.entity;

import com.pcalouche.spat.restservices.AbstractModelMapperTest;
import com.pcalouche.spat.restservices.api.dto.RoleDto;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class RoleTest extends AbstractModelMapperTest {

    @Test
    public void testRoleModelMapper() {
        RoleDto roleDto = new RoleDto(1L, "ROLE_USER");
        Role role = modelMapper.map(roleDto, Role.class);

        assertThat(role.getId()).isEqualTo(1L);
        assertThat(role.getName()).isEqualTo("ROLE_USER");
    }
}
