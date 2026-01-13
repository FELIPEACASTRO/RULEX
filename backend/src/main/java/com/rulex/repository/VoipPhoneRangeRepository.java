package com.rulex.repository;

import com.rulex.entity.VoipPhoneRange;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface VoipPhoneRangeRepository extends JpaRepository<VoipPhoneRange, UUID> {

  @Query(
      "SELECT v FROM VoipPhoneRange v "
          + "WHERE v.countryCode = :countryCode "
          + "AND :phonePrefix LIKE CONCAT(v.prefixStart, '%')")
  List<VoipPhoneRange> findMatchingRanges(
      @Param("countryCode") String countryCode, @Param("phonePrefix") String phonePrefix);

  @Query(
      "SELECT CASE WHEN COUNT(v) > 0 THEN true ELSE false END "
          + "FROM VoipPhoneRange v "
          + "WHERE v.countryCode = :countryCode "
          + "AND v.isVoip = true "
          + "AND :phoneNumber LIKE CONCAT(v.prefixStart, '%')")
  boolean isVoipNumber(
      @Param("countryCode") String countryCode, @Param("phoneNumber") String phoneNumber);

  List<VoipPhoneRange> findByCountryCodeAndIsVoipTrue(String countryCode);
}
