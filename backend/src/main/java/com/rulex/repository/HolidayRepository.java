package com.rulex.repository;

import com.rulex.entity.Holiday;
import java.time.LocalDate;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface HolidayRepository extends JpaRepository<Holiday, UUID> {

  @Query("SELECT h FROM Holiday h WHERE h.holidayDate = :date AND h.countryCode = :countryCode")
  Optional<Holiday> findByDateAndCountry(
      @Param("date") LocalDate date, @Param("countryCode") String countryCode);

  @Query(
      "SELECT CASE WHEN COUNT(h) > 0 THEN true ELSE false END FROM Holiday h "
          + "WHERE h.holidayDate = :date AND h.countryCode = :countryCode")
  boolean isHoliday(@Param("date") LocalDate date, @Param("countryCode") String countryCode);

  @Query(
      "SELECT CASE WHEN COUNT(h) > 0 THEN true ELSE false END FROM Holiday h "
          + "WHERE h.holidayDate = :date AND h.countryCode = :countryCode AND h.isBankHoliday = true")
  boolean isBankHoliday(@Param("date") LocalDate date, @Param("countryCode") String countryCode);
}
