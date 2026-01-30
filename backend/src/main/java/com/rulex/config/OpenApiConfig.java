package com.rulex.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;
import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * GAP-010 FIX: Configuração OpenAPI/Swagger com documentação completa.
 *
 * <p>Adiciona:
 * - Informações da API
 * - Exemplos de request/response
 * - Documentação de erros
 * - Autenticação
 */
@Configuration
public class OpenApiConfig {

  @Value("${spring.application.name:RULEX}")
  private String applicationName;

  @Bean
  public OpenAPI customOpenAPI() {
    return new OpenAPI()
        .info(apiInfo())
        .externalDocs(externalDocs())
        .servers(servers())
        .tags(tags())
        .components(components())
        .addSecurityItem(new SecurityRequirement().addList("basicAuth"));
  }

  private Info apiInfo() {
    return new Info()
        .title("RULEX - Motor de Regras Antifraude")
        .version("2.0.0")
        .description(
            """
            # RULEX API

            Motor de regras duras (hard rules) para detecção de fraude em transações de cartão de crédito.

            ## Características

            - **496 operadores** de avaliação de regras
            - **Decisões determinísticas** e auditáveis
            - **Versionamento** completo de regras
            - **Simulação** de transações
            - **Auditoria** de todas as decisões

            ## Autenticação

            A API usa HTTP Basic Authentication. Credenciais são fornecidas pelo administrador.

            ## Rate Limiting

            - **100 requisições/minuto** por IP (configurável)
            - Header `X-RateLimit-Remaining` indica requisições restantes

            ## Códigos de Erro

            | Código | Descrição |
            |--------|-----------|
            | 400 | Requisição inválida |
            | 401 | Não autenticado |
            | 403 | Não autorizado |
            | 404 | Recurso não encontrado |
            | 413 | Payload muito grande (limite: 1MB) |
            | 429 | Rate limit excedido |
            | 500 | Erro interno |

            ## Decisões

            | Decisão | Descrição |
            |---------|-----------|
            | APROVADO | Transação aprovada |
            | SUSPEITA_DE_FRAUDE | Transação suspeita, requer análise |
            | FRAUDE | Transação bloqueada por fraude |
            """)
        .contact(
            new Contact()
                .name("Equipe RULEX")
                .email("suporte@rulex.com.br")
                .url("https://rulex.com.br"))
        .license(new License().name("Proprietário").url("https://rulex.com.br/licenca"));
  }

  private ExternalDocumentation externalDocs() {
    return new ExternalDocumentation()
        .description("Documentação Completa do RULEX")
        .url("https://docs.rulex.com.br");
  }

  private List<Server> servers() {
    return List.of(
        new Server().url("/api").description("Servidor Local"),
        new Server().url("https://api.rulex.com.br/api").description("Produção"),
        new Server().url("https://staging-api.rulex.com.br/api").description("Staging"));
  }

  private List<Tag> tags() {
    return List.of(
        new Tag()
            .name("Avaliação")
            .description("Endpoints para avaliação de transações e decisão de risco"),
        new Tag()
            .name("Regras")
            .description("CRUD de regras simples"),
        new Tag()
            .name("Regras Complexas")
            .description("CRUD de regras com condições aninhadas"),
        new Tag()
            .name("Simulação")
            .description("Simulação de transações para testes"),
        new Tag()
            .name("Auditoria")
            .description("Consulta de logs de auditoria e decisões"),
        new Tag()
            .name("Operadores")
            .description("Informações sobre operadores disponíveis"),
        new Tag()
            .name("Métricas")
            .description("Métricas de performance e uso"),
        new Tag()
            .name("Homologação")
            .description("Endpoints de versionamento e publicação de regras"));
  }

  private Components components() {
    return new Components()
        .addSecuritySchemes(
            "basicAuth",
            new SecurityScheme()
                .type(SecurityScheme.Type.HTTP)
                .scheme("basic")
                .description("HTTP Basic Authentication"))
        .addExamples("transactionExample", transactionExample())
        .addExamples("evaluateResponseApproved", evaluateResponseApprovedExample())
        .addExamples("evaluateResponseFraud", evaluateResponseFraudExample())
        .addExamples("ruleExample", ruleExample())
        .addResponses("BadRequest", badRequestResponse())
        .addResponses("Unauthorized", unauthorizedResponse())
        .addResponses("RateLimitExceeded", rateLimitResponse());
  }

  private Example transactionExample() {
    return new Example()
        .summary("Transação de exemplo")
        .description("Exemplo de payload de transação para avaliação")
        .value(
            """
            {
              "externalTransactionId": "TX-2025-001",
              "customerIdFromHeader": "CUST-12345",
              "customerAcctNumber": 123456789,
              "pan": "4111111111111111",
              "merchantId": "MERCH-001",
              "merchantName": "Loja Exemplo",
              "merchantCountryCode": "076",
              "merchantCity": "São Paulo",
              "mcc": 5411,
              "transactionAmount": 150.00,
              "transactionCurrencyCode": 986,
              "transactionDate": 20250130,
              "transactionTime": 143052,
              "posEntryMode": "051",
              "consumerAuthenticationScore": 100,
              "externalScore3": 50,
              "cavvResult": 2,
              "eciIndicator": 5,
              "atcCard": 1,
              "atcHost": 1,
              "tokenAssuranceLevel": 0,
              "availableCredit": 5000.00,
              "cardCashBalance": 0.00,
              "cardDelinquentAmount": 0.00
            }
            """);
  }

  private Example evaluateResponseApprovedExample() {
    return new Example()
        .summary("Transação aprovada")
        .description("Exemplo de resposta para transação aprovada")
        .value(
            """
            {
              "transactionId": "TX-2025-001",
              "classification": "APROVADO",
              "riskScore": 15,
              "reason": "Transação dentro dos parâmetros normais",
              "triggeredRules": [],
              "timestamp": "2025-01-30T14:30:52"
            }
            """);
  }

  private Example evaluateResponseFraudExample() {
    return new Example()
        .summary("Transação bloqueada")
        .description("Exemplo de resposta para transação bloqueada por fraude")
        .value(
            """
            {
              "transactionId": "TX-2025-002",
              "classification": "FRAUDE",
              "riskScore": 95,
              "reason": "Múltiplas regras de alto risco disparadas",
              "triggeredRules": [
                {
                  "ruleId": "HIGH_AMOUNT_RULE",
                  "ruleName": "Valor Alto",
                  "score": 40
                },
                {
                  "ruleId": "VELOCITY_RULE",
                  "ruleName": "Velocidade Anormal",
                  "score": 55
                }
              ],
              "timestamp": "2025-01-30T14:31:15"
            }
            """);
  }

  private Example ruleExample() {
    return new Example()
        .summary("Regra de exemplo")
        .description("Exemplo de criação de regra simples")
        .value(
            """
            {
              "ruleName": "HIGH_AMOUNT_RULE",
              "description": "Bloqueia transações acima de R$ 10.000",
              "ruleType": "SECURITY",
              "classification": "SUSPICIOUS",
              "threshold": 0,
              "weight": 50,
              "enabled": true,
              "conditions": [
                {
                  "field": "transactionAmount",
                  "operator": "GT",
                  "value": "10000"
                }
              ],
              "logicOperator": "AND"
            }
            """);
  }

  private ApiResponse badRequestResponse() {
    return new ApiResponse()
        .description("Requisição inválida - verifique o payload")
        .content(
            new Content()
                .addMediaType(
                    "application/json",
                    new MediaType()
                        .example(
                            Map.of(
                                "error", "Bad Request",
                                "message", "Campo 'transactionAmount' é obrigatório",
                                "timestamp", "2025-01-30T14:30:52"))));
  }

  private ApiResponse unauthorizedResponse() {
    return new ApiResponse()
        .description("Não autenticado - forneça credenciais válidas")
        .content(
            new Content()
                .addMediaType(
                    "application/json",
                    new MediaType()
                        .example(
                            Map.of(
                                "error", "Unauthorized",
                                "message", "Credenciais inválidas ou ausentes",
                                "timestamp", "2025-01-30T14:30:52"))));
  }

  private ApiResponse rateLimitResponse() {
    return new ApiResponse()
        .description("Rate limit excedido - aguarde antes de tentar novamente")
        .content(
            new Content()
                .addMediaType(
                    "application/json",
                    new MediaType()
                        .example(
                            Map.of(
                                "error", "Too Many Requests",
                                "message", "Rate limit excedido. Tente novamente em 60 segundos.",
                                "retryAfter", 60,
                                "timestamp", "2025-01-30T14:30:52"))));
  }
}
