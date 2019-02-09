package com.pcalouche.spat.restservices.api.repository;

import com.pcalouche.spat.restservices.api.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRepository extends JpaRepository<User, String> {
}
