package com.rulex.repository;

import com.rulex.entity.GeoReference;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para referências geográficas. */
@Repository
public interface GeoReferenceRepository extends JpaRepository<GeoReference, Long> {

  /** Busca por código do país. */
  List<GeoReference> findByCountryCode(String countryCode);

  /** Busca por código do país e estado. */
  List<GeoReference> findByCountryCodeAndStateCode(String countryCode, String stateCode);

  /** Busca por cidade (case insensitive). */
  @Query("SELECT g FROM GeoReference g WHERE UPPER(g.cityName) = UPPER(:cityName)")
  List<GeoReference> findByCityNameIgnoreCase(@Param("cityName") String cityName);

  /** Busca por país, estado e cidade. */
  @Query(
      "SELECT g FROM GeoReference g WHERE g.countryCode = :countryCode "
          + "AND (:stateCode IS NULL OR g.stateCode = :stateCode) "
          + "AND (:cityName IS NULL OR UPPER(g.cityName) = UPPER(:cityName))")
  Optional<GeoReference> findByLocation(
      @Param("countryCode") String countryCode,
      @Param("stateCode") String stateCode,
      @Param("cityName") String cityName);

  /** Busca a capital de um país. */
  @Query("SELECT g FROM GeoReference g WHERE g.countryCode = :countryCode AND g.isCapital = true")
  Optional<GeoReference> findCapitalByCountryCode(@Param("countryCode") String countryCode);

  /** Busca por código alpha-2 do país (ex: BR, US). */
  List<GeoReference> findByCountryAlpha2(String countryAlpha2);

  /**
   * Busca a referência mais próxima de um ponto (para fallback). Usa distância euclidiana
   * simplificada.
   */
  @Query(
      value =
          "SELECT * FROM geo_reference "
              + "ORDER BY SQRT(POWER(latitude - :lat, 2) + POWER(longitude - :lon, 2)) "
              + "LIMIT 1",
      nativeQuery = true)
  Optional<GeoReference> findNearest(@Param("lat") double lat, @Param("lon") double lon);

  /** Busca referências dentro de um bounding box. */
  @Query(
      "SELECT g FROM GeoReference g WHERE "
          + "g.latitude BETWEEN :minLat AND :maxLat AND "
          + "g.longitude BETWEEN :minLon AND :maxLon")
  List<GeoReference> findWithinBoundingBox(
      @Param("minLat") double minLat,
      @Param("maxLat") double maxLat,
      @Param("minLon") double minLon,
      @Param("maxLon") double maxLon);
}
