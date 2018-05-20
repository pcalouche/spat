package com.pcalouche.spat.restservices.api.role;

import com.pcalouche.spat.restservices.api.entity.Role;
import org.springframework.data.repository.CrudRepository;

public interface RoleRepository extends CrudRepository<Role, Long> {

}
