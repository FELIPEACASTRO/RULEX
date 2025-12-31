package com.rulex.repository;

import com.rulex.entity.GeoPolygon;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * Repositório para polígonos geográficos.
 */
@Repository
public interface GeoPolygonRepository extends JpaRepository<GeoPolygon, Long> {

    /**
     * Busca polígono por nome.
     */
    Optional<GeoPolygon> findByName(String name);

    /**
     * Busca polígonos por tipo.
     */
    List<GeoPolygon> findByPolygonType(String polygonType);

    /**
     * Busca polígonos habilitados.
     */
    List<GeoPolygon> findByEnabledTrue();

    /**
     * Busca polígonos por código do país.
     */
    List<GeoPolygon> findByCountryCode(String countryCode);

    /**
     * Busca polígonos cujo bounding box contém um ponto.
     * Otimização para evitar verificar todos os polígonos.
     */
    @Query("SELECT g FROM GeoPolygon g WHERE g.enabled = true AND " +
           ":lat BETWEEN g.minLat AND g.maxLat AND " +
           ":lon BETWEEN g.minLon AND g.maxLon")
    List<GeoPolygon> findCandidatePolygons(@Param("lat") double lat, @Param("lon") double lon);

    /**
     * Busca polígono por nome (case insensitive).
     */
    @Query("SELECT g FROM GeoPolygon g WHERE UPPER(g.name) = UPPER(:name)")
    Optional<GeoPolygon> findByNameIgnoreCase(@Param("name") String name);
}
