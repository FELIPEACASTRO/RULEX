# Checklist — Homologação Bancária

## P0 (bloqueador)
- [ ] Contrato de erro estruturado em todos endpoints REST (sem `500` vazio)
- [ ] Idempotência comprovada em `/api/transactions/analyze`
- [ ] Persistência e trilha de auditoria gerando evidência (consulta via `/api/audit`)
- [ ] Execução das 28 regras avançadas com evidência de regra disparada
- [ ] Regras configuráveis CRUD completo (create/update/toggle/delete + history)
- [ ] Métricas respondendo consistentemente
- [ ] Homolog DSL: criar/publicar/ativar/simular com evidência

## P1 (importante)
- [ ] tRPC protected/admin com evidência (cookie/sessão)
- [ ] Cenários negativos completos (400/404/409) por grupo
- [ ] Evidência de dados mascarados (PAN) persistidos

## Execução recomendada
- [ ] Rodar `mvn test` no backend
- [ ] Rodar requests do Insomnia em ordem (README do diretório `Insomnia`)
- [ ] Registrar evidências (prints + IDs) e anexar ao relatório de auditoria
