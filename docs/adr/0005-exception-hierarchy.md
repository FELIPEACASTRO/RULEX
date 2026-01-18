# ADR-0005: Exception Hierarchy

## Status
Aceito

## Data
2025-01-15

## Contexto

O código tinha 404 ocorrências de `catch (Exception e)` genérico, causando:

- Erros silenciados ou mal tratados
- Dificuldade de debugging em produção
- Logs pouco informativos
- Respostas de erro inconsistentes para o cliente
- Impossibilidade de tratar erros específicos de forma diferente

## Decisão

Criar hierarquia de exceções específicas do RULEX com tratamento centralizado.

### Hierarquia

```
RulexException (base)
├── RuleEvaluationException      → HTTP 422
├── UnsupportedOperatorException → HTTP 501
├── VelocityCalculationException → HTTP 500
├── GeoServiceException          → HTTP 500
├── Neo4jConnectionException     → HTTP 503
├── RedisConnectionException     → HTTP 503
├── InvalidPayloadException      → HTTP 400
└── ConfigurationException       → HTTP 500
```

### Mapeamento HTTP

| Exceção | HTTP Status | Quando Usar |
|---------|-------------|-------------|
| `UnsupportedOperatorException` | 501 Not Implemented | Operador PLANNED usado |
| `RuleEvaluationException` | 422 Unprocessable Entity | Erro na lógica da regra |
| `InvalidPayloadException` | 400 Bad Request | Payload malformado |
| `Neo4jConnectionException` | 503 Service Unavailable | Neo4j offline |
| `RedisConnectionException` | 503 Service Unavailable | Redis offline |
| `ConfigurationException` | 500 Internal Server Error | Erro de config |
| `RulexException` | 500 Internal Server Error | Erro genérico RULEX |

### GlobalExceptionHandler

Tratamento centralizado em `@RestControllerAdvice`:

```java
@ExceptionHandler(UnsupportedOperatorException.class)
public ResponseEntity<ErrorResponse> handleUnsupportedOperator(...) {
    return ResponseEntity.status(501).body(ErrorResponse.builder()
        .error("Not Implemented")
        .message(ex.getMessage())
        .details(Map.of("operator", ex.getOperator().name()))
        .build());
}
```

### Formato de Resposta

```json
{
  "timestamp": "2025-01-15T10:30:00",
  "status": 501,
  "error": "Not Implemented",
  "message": "Operador 'FATF_CRYPTO_MIXING' não está implementado",
  "path": "/api/evaluate",
  "details": {
    "operator": "FATF_CRYPTO_MIXING",
    "status": "PLANNED",
    "suggestion": "Consulte GET /api/operators/status"
  }
}
```

## Consequências

### Positivas

- Erros específicos com mensagens claras
- Cada tipo de erro pode ser tratado diferentemente
- Logs mais informativos (tipo de erro, contexto)
- Respostas HTTP semânticas (501 vs 500 vs 503)
- Facilita debugging em produção
- Clientes podem tratar erros programaticamente

### Negativas

- Mais código de exceção para manter
- Necessidade de escolher exceção correta ao lançar
- Risco de criar exceções demais

### Regras de Uso

1. **Nunca** usar `catch (Exception e)` genérico
2. Sempre lançar exceção mais específica possível
3. Incluir contexto relevante na mensagem
4. Logar antes de lançar se necessário

## Referências

- [Effective Java - Item 73: Throw exceptions appropriate to the abstraction](https://www.oreilly.com/library/view/effective-java-3rd/9780134686097/)
- [Spring Error Handling Best Practices](https://www.baeldung.com/exception-handling-for-rest-with-spring)
