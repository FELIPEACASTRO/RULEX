package com.rulex.v31.field;

import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface FieldDictionaryRepository extends JpaRepository<FieldDictionaryEntity, UUID> {

  @Query(
      """
      select f
      from FieldDictionaryEntity f
      where (:workflow is null or f.workflow = :workflow)
        and (:recordType is null or f.recordType = :recordType)
        and (:portfolio is null or f.portfolio = :portfolio)
      order by f.jsonPath asc
      """)
  List<FieldDictionaryEntity> findByFilters(
      @Param("workflow") String workflow,
      @Param("recordType") String recordType,
      @Param("portfolio") String portfolio);

  boolean existsByWorkflowAndRecordTypeAndPortfolioAndJsonPath(
      String workflow, String recordType, String portfolio, String jsonPath);
}
