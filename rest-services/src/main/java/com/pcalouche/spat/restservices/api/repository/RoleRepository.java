package com.pcalouche.spat.restservices.api.repository;

import com.pcalouche.spat.restservices.api.entity.Role;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RoleRepository extends JpaRepository<Role, Integer> {
    Role findByName(String name);
}
