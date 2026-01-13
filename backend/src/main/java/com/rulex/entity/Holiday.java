package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

@Entity
@Table(name = "holidays")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Holiday {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "holiday_date", nullable = false)
  private LocalDate holidayDate;

  @Column(name = "holiday_name", nullable = false)
  private String holidayName;

  @Column(name = "country_code", nullable = false, length = 3)
  @Builder.Default
  private String countryCode = "BRA";

  @Column(name = "state_code", length = 2)
  private String stateCode;

  @Column(name = "city_code", length = 10)
  private String cityCode;

  @Column(name = "holiday_type", nullable = false, length = 50)
  @Builder.Default
  private String holidayType = "NATIONAL";

  @Column(name = "is_bank_holiday", nullable = false)
  @Builder.Default
  private Boolean isBankHoliday = true;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;
}
