package com.rulex.v31.feature;

import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface FeatureDefinitionRepository extends JpaRepository<FeatureDefinitionEntity, UUID> {

  List<FeatureDefinitionEntity> findByActiveTrue();

  List<FeatureDefinitionEntity> findByFeatureTypeAndActiveTrue(String featureType);

  List<FeatureDefinitionEntity> findByEntityTypeAndActiveTrue(String entityType);

  @Query(
      """
      SELECT f FROM FeatureDefinitionEntity f
      WHERE f.active = true
        AND (:featureType IS NULL OR f.featureType = :featureType)
        AND (:entityType IS NULL OR f.entityType = :entityType)
        AND (:source IS NULL OR f.source = :source)
      ORDER BY f.featureType, f.featureName
      """)
  List<FeatureDefinitionEntity> findFiltered(
      @Param("featureType") String featureType,
      @Param("entityType") String entityType,
      @Param("source") String source);

  FeatureDefinitionEntity findByFeatureNameAndActiveTrue(String featureName);
}
