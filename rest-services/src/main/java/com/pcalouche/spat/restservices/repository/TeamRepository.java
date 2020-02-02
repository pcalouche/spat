package com.pcalouche.spat.restservices.repository;

import com.pcalouche.spat.restservices.entity.Team;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TeamRepository extends JpaRepository<Team, Integer> {
}
