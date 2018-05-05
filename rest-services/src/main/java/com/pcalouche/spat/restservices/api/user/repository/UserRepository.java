package com.pcalouche.spat.restservices.api.user.repository;

import com.pcalouche.spat.restservices.api.entity.User;
import org.springframework.data.repository.CrudRepository;

public interface UserRepository extends CrudRepository<User, Long> {
}
