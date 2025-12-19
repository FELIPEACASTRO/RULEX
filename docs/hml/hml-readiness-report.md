# Relatório de Prontidão — HML READY

## Status atual
- Backend Java: **OK** (testes passando; contrato de erro estruturado; idempotência implementada em `/transactions/analyze`).
- Backend Node/tRPC: **OK com ressalvas** (procedures protegidas/admin dependem de cookie/sessão e role).

## Evidências técnicas
- Testes Java: `mvn test` (EXIT:0).
- Contrato de erro: `ApiErrorResponse` emitido por `GlobalExceptionHandler`.
- Idempotência: `externalTransactionId` tratado como chave canônica (repetição retorna decisão persistida; race-safe via captura de `DataIntegrityViolationException`).
- Determinismo de timestamps: serviços usam `Clock`; entidades não sobrescrevem timestamps predefinidos.

## Riscos / pontos de atenção
- tRPC admin: validação de `system.notifyOwner` exige `ctx.user.role === "admin"`; sem seed/controle disso, a evidência pode ficar manual.
- OAuth callback em dev/hml: depende de provedor externo (ou mock); para evidência de homologação, registrar procedimento de obtenção de cookie.

## Recomendações para HML
- Padronizar ambiente de execução:
  - Java API: `http://localhost:8080/api`
  - Node: `http://localhost:3000`
- Executar o projeto Insomnia em `./Insomnia/` como baseline de evidência, coletando:
  - prints de requests/responses para P0
  - logs/ids de transação para rastreabilidade
