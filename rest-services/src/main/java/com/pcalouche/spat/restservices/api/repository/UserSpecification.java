package com.pcalouche.spat.restservices.api.repository;

import com.pcalouche.spat.restservices.api.entity.Role;
import com.pcalouche.spat.restservices.api.entity.User;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;

public class UserSpecification implements Specification<User> {
    private final String username;

    public UserSpecification(String username) {
        super();
        this.username = username;
    }

    @Override
    public Predicate toPredicate(Root<User> root, CriteriaQuery<?> cq, CriteriaBuilder cb) {
        Join<User, Role> joinRole = root.join("roles");
        return cb.and(
                cb.like(root.get("username"), "%" + username + "%"),
                cb.equal(root.get("enabled"), true),
                cb.equal(joinRole.get("name"), "ROLE_ADMIN"));
    }
}
