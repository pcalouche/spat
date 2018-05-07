package com.pcalouche.spat.restservices.api.team.repository;

import com.pcalouche.spat.restservices.api.entity.Team;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TeamRepository extends CrudRepository<Team, Long> {
}
