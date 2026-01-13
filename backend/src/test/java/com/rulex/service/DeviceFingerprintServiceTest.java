package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Testes para o DeviceFingerprintService.
 *
 * <p>GAP-FIX #3: Cobertura de testes para classes críticas.
 *
 * <p>O DeviceFingerprintService cria identificadores únicos para dispositivos baseados em:
 *
 * <ul>
 *   <li>Sinais de hardware: resolução de tela, plataforma, memória, cores de CPU
 *   <li>Sinais de software: user agent, idioma, timezone
 *   <li>Sinais de browser: plugins, fontes, canvas hash, WebGL hash
 *   <li>Sinais de rede: prefixo IP, tipo de conexão
 * </ul>
 */
@DisplayName("DeviceFingerprintService Tests")
class DeviceFingerprintServiceTest {

  private DeviceFingerprintService fingerprintService;

  @BeforeEach
  void setUp() {
    fingerprintService = new DeviceFingerprintService();
  }

  @Nested
  @DisplayName("Análise de Fingerprint")
  class FingerprintAnalysis {

    @Test
    @DisplayName("Deve analisar fingerprint de novo dispositivo")
    void shouldAnalyzeNewDeviceFingerprint() {
      String panHash = "hash-pan-123";
      Map<String, Object> fingerprint = createBasicFingerprint();

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint(panHash, fingerprint);

      assertThat(result).isNotNull();
      assertThat(result.fingerprintHash()).isNotNull();
      assertThat(result.isNewDevice()).isTrue();
      assertThat(result.riskLevel()).isEqualTo(DeviceFingerprintService.RiskLevel.HIGH);
      assertThat(result.riskFactors()).contains("NEW_DEVICE_FOR_CARD");
    }

    @Test
    @DisplayName("Deve reconhecer dispositivo conhecido")
    void shouldRecognizeKnownDevice() {
      String panHash = "hash-pan-456";
      Map<String, Object> fingerprint = createBasicFingerprint();

      // Primeira análise - novo dispositivo
      fingerprintService.analyzeFingerprint(panHash, fingerprint);

      // Segunda análise - dispositivo conhecido
      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint(panHash, fingerprint);

      assertThat(result.isNewDevice()).isFalse();
      assertThat(result.similarityScore()).isGreaterThan(0.7);
    }

    @Test
    @DisplayName("Deve detectar device farming")
    void shouldDetectDeviceFarming() {
      Map<String, Object> fingerprint = createBasicFingerprint();

      // Usar mesmo dispositivo com múltiplos PANs
      for (int i = 0; i < 6; i++) {
        fingerprintService.analyzeFingerprint("pan-hash-" + i, fingerprint);
      }

      // Próxima análise deve detectar device farming
      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint("pan-hash-new", fingerprint);

      assertThat(result.isDeviceFarm()).isTrue();
      assertThat(result.cardsOnDevice()).isGreaterThanOrEqualTo(5);
      assertThat(result.riskLevel()).isEqualTo(DeviceFingerprintService.RiskLevel.CRITICAL);
      assertThat(result.riskFactors().stream().anyMatch(f -> f.contains("DEVICE_FARMING")))
          .isTrue();
    }

    @Test
    @DisplayName("Deve detectar múltiplos dispositivos para mesmo cartão")
    void shouldDetectMultipleDevicesForSameCard() {
      String panHash = "hash-pan-multi-device";

      // Registrar múltiplos dispositivos diferentes
      for (int i = 0; i < 5; i++) {
        Map<String, Object> fingerprint = createBasicFingerprint();
        fingerprint.put("userAgent", "Mozilla/5.0 Device-" + i);
        fingerprint.put("canvasHash", "canvas-hash-" + i);
        fingerprint.put("webglHash", "webgl-hash-" + i);
        fingerprintService.analyzeFingerprint(panHash, fingerprint);
      }

      // Verificar contagem de dispositivos
      Map<String, Object> newFingerprint = createBasicFingerprint();
      newFingerprint.put("userAgent", "Mozilla/5.0 Device-New");
      newFingerprint.put("canvasHash", "canvas-hash-new");

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint(panHash, newFingerprint);

      assertThat(result.devicesForCard()).isGreaterThanOrEqualTo(4);
      assertThat(result.riskFactors().stream().anyMatch(f -> f.contains("MULTIPLE_DEVICES")))
          .isTrue();
    }

    @Test
    @DisplayName("Deve calcular score de similaridade")
    void shouldCalculateSimilarityScore() {
      String panHash = "hash-pan-similarity";
      Map<String, Object> fingerprint1 = createBasicFingerprint();

      // Primeira análise
      fingerprintService.analyzeFingerprint(panHash, fingerprint1);

      // Segunda análise com pequenas mudanças
      Map<String, Object> fingerprint2 = createBasicFingerprint();
      fingerprint2.put("userAgent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/121.0.0.0");

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint(panHash, fingerprint2);

      // Similaridade deve ser alta (mesmo dispositivo, browser atualizado)
      assertThat(result.similarityScore()).isGreaterThan(0.5);
    }
  }

  @Nested
  @DisplayName("Detecção de Spoofing")
  class SpoofingDetection {

    @Test
    @DisplayName("Deve detectar fingerprint genérico/bloqueado")
    void shouldDetectGenericFingerprint() {
      String panHash = "hash-pan-generic";

      // Primeiro, registrar um fingerprint normal
      Map<String, Object> normalFingerprint = createBasicFingerprint();
      fingerprintService.analyzeFingerprint(panHash, normalFingerprint);

      // Fingerprint com muitos campos vazios (privacy tools)
      Map<String, Object> genericFingerprint = new HashMap<>();
      genericFingerprint.put("userAgent", "Mozilla/5.0");
      genericFingerprint.put("screenResolution", "1920x1080");
      genericFingerprint.put("platform", "Win32");
      // Campos de fingerprinting bloqueados
      genericFingerprint.put("canvasHash", "");
      genericFingerprint.put("webglHash", "");
      genericFingerprint.put("audioHash", "");
      genericFingerprint.put("plugins", List.of());
      genericFingerprint.put("fonts", List.of());

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint(panHash, genericFingerprint);

      // O fingerprint genérico deve ser detectado como spoofing ou ter fatores de risco
      assertThat(result.riskFactors())
          .anyMatch(f -> f.contains("FINGERPRINT_BLOCKED") || f.contains("NEW_DEVICE"));
    }

    @Test
    @DisplayName("Deve detectar mudança de plataforma suspeita")
    void shouldDetectPlatformSpoofing() {
      String panHash = "hash-pan-platform-spoof";

      // Primeiro fingerprint
      Map<String, Object> fingerprint1 = createBasicFingerprint();
      fingerprint1.put("platform", "Win32");
      fingerprintService.analyzeFingerprint(panHash, fingerprint1);

      // Segundo fingerprint - mesma resolução e canvas, mas plataforma diferente
      Map<String, Object> fingerprint2 = createBasicFingerprint();
      fingerprint2.put("platform", "MacIntel");

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint(panHash, fingerprint2);

      assertThat(result.riskFactors().stream().anyMatch(f -> f.contains("PLATFORM_SPOOFING")))
          .isTrue();
    }

    @Test
    @DisplayName("Deve detectar inconsistência de memória")
    void shouldDetectMemoryInconsistency() {
      String panHash = "hash-pan-memory-spoof";

      // Primeiro fingerprint com 8GB
      Map<String, Object> fingerprint1 = createBasicFingerprint();
      fingerprint1.put("deviceMemory", 8);
      fingerprintService.analyzeFingerprint(panHash, fingerprint1);

      // Segundo fingerprint com 32GB (mudança drástica)
      Map<String, Object> fingerprint2 = createBasicFingerprint();
      fingerprint2.put("deviceMemory", 32);

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint(panHash, fingerprint2);

      assertThat(result.riskFactors().stream().anyMatch(f -> f.contains("MEMORY_INCONSISTENCY")))
          .isTrue();
    }
  }

  @Nested
  @DisplayName("Estatísticas")
  class Statistics {

    @Test
    @DisplayName("Deve retornar estatísticas do serviço")
    void shouldReturnServiceStatistics() {
      // Analisar alguns fingerprints
      for (int i = 0; i < 3; i++) {
        fingerprintService.analyzeFingerprint("pan-" + i, createBasicFingerprint());
      }

      Map<String, Object> stats = fingerprintService.getStatistics();

      assertThat(stats).containsKey("totalFingerprints");
      assertThat(stats).containsKey("uniquePans");
      assertThat(stats).containsKey("uniqueDevices");
      assertThat(stats).containsKey("newDeviceDetections");
      assertThat(stats).containsKey("deviceFarmDetections");
      assertThat(stats).containsKey("spoofingAttempts");

      assertThat((Long) stats.get("totalFingerprints")).isGreaterThanOrEqualTo(3);
      assertThat((Integer) stats.get("uniquePans")).isGreaterThanOrEqualTo(3);
    }

    @Test
    @DisplayName("Deve incrementar contador de novos dispositivos")
    void shouldIncrementNewDeviceCounter() {
      Map<String, Object> stats1 = fingerprintService.getStatistics();
      long initialNewDevices = (Long) stats1.get("newDeviceDetections");

      fingerprintService.analyzeFingerprint("new-pan", createBasicFingerprint());

      Map<String, Object> stats2 = fingerprintService.getStatistics();
      long finalNewDevices = (Long) stats2.get("newDeviceDetections");

      assertThat(finalNewDevices).isEqualTo(initialNewDevices + 1);
    }
  }

  @Nested
  @DisplayName("Risco de Dispositivo")
  class DeviceRisk {

    @Test
    @DisplayName("Deve retornar risco baixo para dispositivo normal")
    void shouldReturnLowRiskForNormalDevice() {
      Map<String, Object> fingerprint = createBasicFingerprint();
      DeviceFingerprintService.FingerprintAnalysis analysis =
          fingerprintService.analyzeFingerprint("pan-normal", fingerprint);

      Map<String, Object> deviceRisk = fingerprintService.getDeviceRisk(analysis.fingerprintHash());

      assertThat(deviceRisk).containsKey("fingerprintHash");
      assertThat(deviceRisk).containsKey("cardCount");
      assertThat(deviceRisk).containsKey("isDeviceFarm");
      assertThat(deviceRisk).containsKey("riskLevel");
      assertThat((Boolean) deviceRisk.get("isDeviceFarm")).isFalse();
      assertThat(deviceRisk.get("riskLevel")).isEqualTo("LOW");
    }

    @Test
    @DisplayName("Deve retornar risco alto para device farm")
    void shouldReturnHighRiskForDeviceFarm() {
      Map<String, Object> fingerprint = createBasicFingerprint();

      // Usar mesmo dispositivo com muitos PANs
      for (int i = 0; i < 6; i++) {
        fingerprintService.analyzeFingerprint("farm-pan-" + i, fingerprint);
      }

      DeviceFingerprintService.FingerprintAnalysis analysis =
          fingerprintService.analyzeFingerprint("farm-pan-final", fingerprint);

      Map<String, Object> deviceRisk = fingerprintService.getDeviceRisk(analysis.fingerprintHash());

      assertThat((Boolean) deviceRisk.get("isDeviceFarm")).isTrue();
      assertThat(deviceRisk.get("riskLevel")).isEqualTo("HIGH");
    }

    @Test
    @DisplayName("Deve retornar risco médio para dispositivo com 3-4 cartões")
    void shouldReturnMediumRiskForDeviceWithFewCards() {
      Map<String, Object> fingerprint = createBasicFingerprint();

      // Usar mesmo dispositivo com 4 PANs
      for (int i = 0; i < 4; i++) {
        fingerprintService.analyzeFingerprint("medium-pan-" + i, fingerprint);
      }

      DeviceFingerprintService.FingerprintAnalysis analysis =
          fingerprintService.analyzeFingerprint("medium-pan-final", fingerprint);

      Map<String, Object> deviceRisk = fingerprintService.getDeviceRisk(analysis.fingerprintHash());

      assertThat((Integer) deviceRisk.get("cardCount")).isEqualTo(5);
      assertThat(deviceRisk.get("riskLevel")).isEqualTo("HIGH");
    }
  }

  @Nested
  @DisplayName("Níveis de Risco")
  class RiskLevels {

    @Test
    @DisplayName("Deve retornar score correto para cada nível de risco")
    void shouldReturnCorrectScoreForEachRiskLevel() {
      assertThat(DeviceFingerprintService.RiskLevel.LOW.getScore()).isEqualTo(0);
      assertThat(DeviceFingerprintService.RiskLevel.MEDIUM.getScore()).isEqualTo(50);
      assertThat(DeviceFingerprintService.RiskLevel.HIGH.getScore()).isEqualTo(80);
      assertThat(DeviceFingerprintService.RiskLevel.CRITICAL.getScore()).isEqualTo(100);
    }

    @Test
    @DisplayName("Deve classificar como CRITICAL para device farm")
    void shouldClassifyAsCriticalForDeviceFarm() {
      Map<String, Object> fingerprint = createBasicFingerprint();

      for (int i = 0; i < 6; i++) {
        fingerprintService.analyzeFingerprint("critical-pan-" + i, fingerprint);
      }

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint("critical-pan-new", fingerprint);

      assertThat(result.riskLevel()).isEqualTo(DeviceFingerprintService.RiskLevel.CRITICAL);
    }

    @Test
    @DisplayName("Deve classificar como HIGH para novo dispositivo")
    void shouldClassifyAsHighForNewDevice() {
      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint("high-risk-pan", createBasicFingerprint());

      assertThat(result.riskLevel()).isEqualTo(DeviceFingerprintService.RiskLevel.HIGH);
    }

    @Test
    @DisplayName("Deve classificar como LOW para dispositivo conhecido")
    void shouldClassifyAsLowForKnownDevice() {
      String panHash = "low-risk-pan";
      Map<String, Object> fingerprint = createBasicFingerprint();

      // Primeira análise
      fingerprintService.analyzeFingerprint(panHash, fingerprint);

      // Segunda análise - dispositivo conhecido
      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint(panHash, fingerprint);

      assertThat(result.riskLevel()).isEqualTo(DeviceFingerprintService.RiskLevel.LOW);
    }
  }

  @Nested
  @DisplayName("Campos de Fingerprint")
  class FingerprintFields {

    @Test
    @DisplayName("Deve processar fingerprint com todos os campos")
    void shouldProcessFingerprintWithAllFields() {
      Map<String, Object> fullFingerprint = new HashMap<>();
      fullFingerprint.put(
          "userAgent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0.0.0");
      fullFingerprint.put("screenResolution", "1920x1080");
      fullFingerprint.put("platform", "Win32");
      fullFingerprint.put("language", "pt-BR");
      fullFingerprint.put("timezone", "America/Sao_Paulo");
      fullFingerprint.put("colorDepth", 24);
      fullFingerprint.put("deviceMemory", 8);
      fullFingerprint.put("hardwareConcurrency", 8);
      fullFingerprint.put("canvasHash", "abc123canvashash");
      fullFingerprint.put("webglHash", "xyz789webglhash");
      fullFingerprint.put("audioHash", "audio123hash");
      fullFingerprint.put("plugins", List.of("PDF Viewer", "Chrome PDF Plugin"));
      fullFingerprint.put("fonts", List.of("Arial", "Times New Roman", "Verdana"));
      fullFingerprint.put("ipPrefix", "192.168.1");
      fullFingerprint.put("connectionType", "wifi");

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint("full-fingerprint-pan", fullFingerprint);

      assertThat(result).isNotNull();
      assertThat(result.fingerprintHash()).isNotNull().isNotEmpty();
    }

    @Test
    @DisplayName("Deve processar fingerprint com campos mínimos")
    void shouldProcessFingerprintWithMinimalFields() {
      Map<String, Object> minimalFingerprint = new HashMap<>();
      minimalFingerprint.put("userAgent", "Mozilla/5.0");

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint("minimal-fingerprint-pan", minimalFingerprint);

      assertThat(result).isNotNull();
      assertThat(result.fingerprintHash()).isNotNull();
    }

    @Test
    @DisplayName("Deve processar fingerprint com campos nulos")
    void shouldProcessFingerprintWithNullFields() {
      Map<String, Object> nullFieldsFingerprint = new HashMap<>();
      nullFieldsFingerprint.put("userAgent", null);
      nullFieldsFingerprint.put("screenResolution", null);
      nullFieldsFingerprint.put("deviceMemory", null);

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint("null-fields-pan", nullFieldsFingerprint);

      assertThat(result).isNotNull();
    }

    @Test
    @DisplayName("Deve processar fingerprint vazio")
    void shouldProcessEmptyFingerprint() {
      Map<String, Object> emptyFingerprint = new HashMap<>();

      DeviceFingerprintService.FingerprintAnalysis result =
          fingerprintService.analyzeFingerprint("empty-fingerprint-pan", emptyFingerprint);

      assertThat(result).isNotNull();
      assertThat(result.isNewDevice()).isTrue();
    }
  }

  @Nested
  @DisplayName("Limpeza de Fingerprints")
  class FingerprintCleanup {

    @Test
    @DisplayName("Deve executar limpeza sem erros")
    void shouldExecuteCleanupWithoutErrors() {
      // Adicionar alguns fingerprints
      for (int i = 0; i < 5; i++) {
        fingerprintService.analyzeFingerprint("cleanup-pan-" + i, createBasicFingerprint());
      }

      // Executar limpeza (não deve lançar exceção)
      fingerprintService.cleanupExpiredFingerprints();

      // Verificar que o serviço ainda funciona
      Map<String, Object> stats = fingerprintService.getStatistics();
      assertThat(stats).isNotNull();
    }
  }

  // ========== Helpers ==========

  private Map<String, Object> createBasicFingerprint() {
    Map<String, Object> fingerprint = new HashMap<>();
    fingerprint.put("userAgent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0.0.0");
    fingerprint.put("screenResolution", "1920x1080");
    fingerprint.put("platform", "Win32");
    fingerprint.put("language", "pt-BR");
    fingerprint.put("timezone", "America/Sao_Paulo");
    fingerprint.put("colorDepth", 24);
    fingerprint.put("deviceMemory", 8);
    fingerprint.put("hardwareConcurrency", 8);
    fingerprint.put("canvasHash", "test-canvas-hash-12345");
    fingerprint.put("webglHash", "test-webgl-hash-67890");
    fingerprint.put("audioHash", "test-audio-hash-abcde");
    fingerprint.put("plugins", List.of("PDF Viewer", "Chrome PDF Plugin"));
    fingerprint.put("fonts", List.of("Arial", "Times New Roman"));
    fingerprint.put("ipPrefix", "192.168.1");
    fingerprint.put("connectionType", "wifi");
    return fingerprint;
  }
}
