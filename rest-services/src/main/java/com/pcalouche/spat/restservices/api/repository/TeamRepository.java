package com.pcalouche.spat.restservices.api.repository;

import com.pcalouche.spat.restservices.api.entity.Team;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TeamRepository extends JpaRepository<Team, Integer> {
}
