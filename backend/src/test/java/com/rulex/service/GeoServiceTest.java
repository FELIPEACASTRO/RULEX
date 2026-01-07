package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.GeoReference;
import com.rulex.repository.GeoPolygonRepository;
import com.rulex.repository.GeoReferenceRepository;
import com.rulex.service.GeoService.GeoCoordinates;
import java.math.BigDecimal;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@DisplayName("GeoService")
class GeoServiceTest {

  @Mock private GeoReferenceRepository geoReferenceRepository;
  @Mock private GeoPolygonRepository geoPolygonRepository;

  private GeoService geoService;
  private ObjectMapper objectMapper;

  @BeforeEach
  void setUp() {
    objectMapper = new ObjectMapper();
    geoService = new GeoService(geoReferenceRepository, geoPolygonRepository, objectMapper);
  }

  @Nested
  @DisplayName("deriveCoordinates")
  class DeriveCoordinatesTests {

    @Test
    @DisplayName("deve retornar not found para request null")
    void shouldReturnNotFoundForNullRequest() {
      GeoCoordinates result = geoService.deriveCoordinates(null);

      assertThat(result.isFound()).isFalse();
      assertThat(result.getSource()).isEqualTo("NOT_FOUND");
    }

    @Test
    @DisplayName("deve retornar not found para país ausente")
    void shouldReturnNotFoundForMissingCountry() {
      TransactionRequest request = TransactionRequest.builder().merchantCity("São Paulo").build();

      GeoCoordinates result = geoService.deriveCoordinates(request);

      assertThat(result.isFound()).isFalse();
    }

    @Test
    @DisplayName("deve encontrar coordenadas por cidade")
    void shouldFindCoordinatesByCity() {
      TransactionRequest request =
          TransactionRequest.builder()
              .merchantCountryCode("BR")
              .merchantState("SP")
              .merchantCity("São Paulo")
              .build();

      GeoReference geoRef = new GeoReference();
      geoRef.setLatitude(BigDecimal.valueOf(-23.5505));
      geoRef.setLongitude(BigDecimal.valueOf(-46.6333));

      when(geoReferenceRepository.findByLocation(anyString(), anyString(), anyString()))
          .thenReturn(Optional.of(geoRef));

      GeoCoordinates result = geoService.deriveCoordinates(request);

      assertThat(result.isFound()).isTrue();
      assertThat(result.getLatitude()).isEqualTo(-23.5505);
      assertThat(result.getLongitude()).isEqualTo(-46.6333);
    }

    @Test
    @DisplayName("deve usar fallback para capital quando cidade não encontrada")
    void shouldFallbackToCapitalWhenCityNotFound() {
      TransactionRequest request =
          TransactionRequest.builder()
              .merchantCountryCode("BR")
              .merchantState("SP")
              .merchantCity("Cidade Inexistente")
              .build();

      GeoReference capital = new GeoReference();
      capital.setLatitude(BigDecimal.valueOf(-15.7801));
      capital.setLongitude(BigDecimal.valueOf(-47.9292));

      when(geoReferenceRepository.findByLocation(anyString(), anyString(), anyString()))
          .thenReturn(Optional.empty());
      when(geoReferenceRepository.findCapitalByCountryCode("BR")).thenReturn(Optional.of(capital));

      GeoCoordinates result = geoService.deriveCoordinates(request);

      assertThat(result.isFound()).isTrue();
      assertThat(result.getSource()).isEqualTo("COUNTRY_CAPITAL");
    }
  }
}
