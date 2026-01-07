package com.rulex.repository;

import com.rulex.entity.BinLookup;
import java.util.List;
import java.util.Optional;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para lookup de BIN. */
@Repository
public interface BinLookupRepository extends JpaRepository<BinLookup, Long> {

  /** Busca BIN exato. */
  @Cacheable(value = "binLookup", key = "#bin")
  Optional<BinLookup> findByBin(String bin);

  /**
   * Busca BIN por prefixo (para BINs de 6 ou 8 dígitos). Tenta primeiro com 8 dígitos, depois com
   * 6.
   */
  @Cacheable(value = "binLookup", key = "'prefix:' + #bin8 + ':' + #bin6")
  @Query(
      "SELECT b FROM BinLookup b WHERE b.bin = :bin8 OR b.bin = :bin6 ORDER BY LENGTH(b.bin) DESC")
  List<BinLookup> findByBinPrefix(@Param("bin8") String bin8, @Param("bin6") String bin6);

  /** Busca todos os BINs de uma bandeira. */
  List<BinLookup> findByCardBrand(String cardBrand);

  /** Busca todos os BINs de um país. */
  List<BinLookup> findByIssuerCountry(String issuerCountry);

  /** Busca todos os BINs de um país (código numérico). */
  List<BinLookup> findByIssuerCountryNumeric(String issuerCountryNumeric);

  /** Verifica se um BIN é de cartão pré-pago. */
  @Query("SELECT COALESCE(b.isPrepaid, false) FROM BinLookup b WHERE b.bin = :bin")
  Boolean isPrepaid(@Param("bin") String bin);

  /** Verifica se um BIN é de cartão comercial. */
  @Query("SELECT COALESCE(b.isCommercial, false) FROM BinLookup b WHERE b.bin = :bin")
  Boolean isCommercial(@Param("bin") String bin);
}
