# fonte/

Esta pasta guarda artefatos de pesquisa/auditoria e exports de regras.

## Arquivos

- `URLS_EXTRACTED.txt`: URLs extraídas/deduplicadas a partir de documentos.
- `GITHUB_REPOS_EXTRACTED.txt`: repositórios GitHub coletados na pesquisa.
- `ANALISE_URLS_P0_STANDARDS_REGULATORS.md`: análise focada em padrões/reguladores (evidência).

## PROMPT ÚNICO — motor de regras duras

- `PROMPT_UNICO_MOTOR_DE_FRAUDE_POR_REGRA_DURA.md`: contrato de saída (restrições do prompt) + status do bloqueador.
- `EXPORT_REGRAS_DURAS_ADVANCED_FROM_CODE.yaml`: export determinístico (1 regra por documento YAML) extraído do backend.

## Pesquisa devastadora (corpus de links)

- `pesquisa_devastadora_fraude_cartoes_web_2025-12-23.md`: relatório consolidado de fontes (curadoria).
- `todas_urls_pesquisa_fraude_cartoes.md`: compilação completa das URLs.
- `todas_urls_pesquisa_fraude_cartoes_enriquecido.md`: compilação + enriquecimento (datasets/features/templates).
- `prompt_gpt52_analise_devastadora_rulex.md`: prompt master para análise exaustiva das URLs.

## Observação importante

A frase final obrigatória do "PROMPT ÚNICO" ainda não foi localizada no repositório/histórico.
Quando você colar a frase exata, ela será incorporada no output final conforme o contrato.
