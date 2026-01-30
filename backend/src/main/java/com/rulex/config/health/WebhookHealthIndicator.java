package com.rulex.config.health;

import com.rulex.service.WebhookClient;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.stereotype.Component;

/**
 * GAP-012 FIX: Health check para endpoints de webhook configurados.
 *
 * <p>Verifica se os webhooks configurados estão acessíveis. Útil para detectar problemas de
 * conectividade com serviços externos antes que afetem transações.
 */
@Component("webhookHealth")
@RequiredArgsConstructor
@Slf4j
public class WebhookHealthIndicator implements HealthIndicator {

  private final WebhookClient webhookClient;

  @Value("${rulex.webhook.enabled:true}")
  private boolean webhooksEnabled;

  @Value("${rulex.webhook.health-check-url:}")
  private String healthCheckUrl;

  @Override
  public Health health() {
    if (!webhooksEnabled) {
      return Health.up()
          .withDetail("status", "DISABLED")
          .withDetail("message", "Webhooks estão desabilitados")
          .build();
    }

    if (healthCheckUrl == null || healthCheckUrl.isBlank()) {
      return Health.up()
          .withDetail("status", "NOT_CONFIGURED")
          .withDetail("message", "Nenhum webhook configurado para health check")
          .build();
    }

    try {
      boolean reachable = webhookClient.isWebhookReachable(healthCheckUrl);

      if (reachable) {
        return Health.up()
            .withDetail("status", "REACHABLE")
            .withDetail("url", maskUrl(healthCheckUrl))
            .build();
      } else {
        return Health.down()
            .withDetail("status", "UNREACHABLE")
            .withDetail("url", maskUrl(healthCheckUrl))
            .withDetail("message", "Webhook não está respondendo")
            .build();
      }
    } catch (Exception e) {
      log.error("Erro ao verificar health do webhook: {}", e.getMessage());
      return Health.down()
          .withDetail("status", "ERROR")
          .withDetail("url", maskUrl(healthCheckUrl))
          .withDetail("error", e.getMessage())
          .build();
    }
  }

  /**
   * Mascara a URL para não expor informações sensíveis nos logs/health.
   */
  private String maskUrl(String url) {
    if (url == null || url.length() < 20) {
      return "***";
    }
    // Mostra apenas o domínio
    try {
      java.net.URI uri = java.net.URI.create(url);
      return uri.getScheme() + "://" + uri.getHost() + "/***";
    } catch (Exception e) {
      return url.substring(0, 10) + "***";
    }
  }
}
