package com.rulex.repository;

import com.rulex.entity.CustomerBeneficiaryHistory;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface CustomerBeneficiaryHistoryRepository
    extends JpaRepository<CustomerBeneficiaryHistory, UUID> {

  List<CustomerBeneficiaryHistory> findByCustomerId(String customerId);

  Optional<CustomerBeneficiaryHistory> findByCustomerIdAndBeneficiaryId(
      String customerId, String beneficiaryId);

  @Query(
      "SELECT CASE WHEN COUNT(b) > 0 THEN true ELSE false END "
          + "FROM CustomerBeneficiaryHistory b "
          + "WHERE b.customerId = :customerId AND b.beneficiaryId = :beneficiaryId")
  boolean existsBeneficiaryForCustomer(
      @Param("customerId") String customerId, @Param("beneficiaryId") String beneficiaryId);

  @Query(
      "SELECT CASE WHEN COUNT(b) > 0 THEN true ELSE false END "
          + "FROM CustomerBeneficiaryHistory b "
          + "WHERE b.customerId = :customerId "
          + "AND b.beneficiaryId = :beneficiaryId "
          + "AND b.isTrusted = true")
  boolean isTrustedBeneficiary(
      @Param("customerId") String customerId, @Param("beneficiaryId") String beneficiaryId);

  @Query("SELECT COUNT(b) FROM CustomerBeneficiaryHistory b WHERE b.customerId = :customerId")
  long countBeneficiariesByCustomer(@Param("customerId") String customerId);

  @Query(
      "SELECT b.beneficiaryId FROM CustomerBeneficiaryHistory b "
          + "WHERE b.customerId = :customerId")
  List<String> findBeneficiaryIdsByCustomer(@Param("customerId") String customerId);
}
