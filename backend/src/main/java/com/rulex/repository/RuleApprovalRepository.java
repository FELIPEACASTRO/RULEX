package com.rulex.repository;

import com.rulex.entity.RuleApproval;
import com.rulex.entity.RuleApproval.ApprovalStatus;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para aprovações de regras (workflow 4 olhos). */
@Repository
public interface RuleApprovalRepository extends JpaRepository<RuleApproval, Long> {

  /** Busca aprovações pendentes. */
  List<RuleApproval> findByStatusOrderByRequestedAtDesc(ApprovalStatus status);

  /** Busca aprovações pendentes paginadas. */
  Page<RuleApproval> findByStatus(ApprovalStatus status, Pageable pageable);

  /** Busca aprovações por regra. */
  List<RuleApproval> findByRuleIdOrderByRequestedAtDesc(Long ruleId);

  /** Busca aprovações solicitadas por um usuário. */
  List<RuleApproval> findByRequestedByOrderByRequestedAtDesc(String requestedBy);

  /** Busca aprovação pendente para uma regra específica. */
  Optional<RuleApproval> findByRuleIdAndStatus(Long ruleId, ApprovalStatus status);

  /** Conta aprovações pendentes. */
  long countByStatus(ApprovalStatus status);

  /** Verifica se existe aprovação pendente para uma regra. */
  boolean existsByRuleIdAndStatus(Long ruleId, ApprovalStatus status);

  /** Busca aprovações por período. */
  @Query(
      "SELECT a FROM RuleApproval a WHERE a.requestedAt >= :startDate AND a.requestedAt <= :endDate ORDER BY a.requestedAt DESC")
  List<RuleApproval> findByPeriod(
      @Param("startDate") java.time.LocalDateTime startDate,
      @Param("endDate") java.time.LocalDateTime endDate);

  /** Busca aprovações aprovadas por um usuário. */
  List<RuleApproval> findByApprovedByOrderByApprovedAtDesc(String approvedBy);
}
