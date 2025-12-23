# PROMPT ÚNICO — MOTOR DE FRAUDE POR REGRA DURA (Contrato de Saída)

## Objetivo
Gerar regras determinísticas (regras duras) para o RULEX, com **limiares (thresholds) apenas quando explicitamente presentes em evidências**.

## Regras obrigatórias de saída
- A resposta final deve conter **APENAS blocos YAML**.
- Deve haver **1 regra por bloco YAML** (1 documento YAML por regra, separado por `---`).
- Não usar ML, heurísticas, scorecards probabilísticos, ou “sugestões” sem evidência.
- Se um threshold/limiar não estiver explicitamente em fonte verificável, **descartar** a regra ou remover o threshold.

## Evidência
- Para cada regra, incluir `evidence_sources` apontando para fontes verificáveis (ex.: arquivos do repo, testes, documentação, URLs).

## Frase final obrigatória (BLOQUEADOR)
A conversa exige que a resposta termine com uma frase de encerramento **exata**.

- Status: **NÃO ENCONTRADA** no working tree e nem por busca no histórico git.
- Ação necessária: colar aqui a frase final obrigatória exatamente como deve aparecer.

FRASE_FINAL_EXATA: "__COLE_AQUI_A_FRASE_FINAL_EXATA_DO_PROMPT_UNICO__"
