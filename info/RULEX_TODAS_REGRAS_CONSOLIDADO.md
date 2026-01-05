# RULEX — Todas as Regras Geradas (Consolidadas)

**Gerado em:** 2026-01-05 09:00 (America/Sao_Paulo)

Este arquivo consolida **absolutamente todas as regras** que foram geradas nas análises anteriores nesta conversa, incluindo:
- regras do catálogo gerado a partir das URLs (`RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md`);
- regras/itens adicionais descritos em texto (marcadas como **legado**);
- caso existam blocos idênticos, eles aparecem uma vez no corpo e são referenciados no índice como duplicados exatos.

## Índice (todas as regras)

| # | ID | Título | Origem | Hash |
|---:|---|---|---|---|
| 1 | AML-001 | High-risk country/jurisdiction (FATF list) + transações recorrentes | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `5f0ee893ac` |
| 2 | AML-002 | TBML proxy: pagamentos comerciais incoerentes (valor x perfil) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `c09400a572` |
| 3 | AML-003 | Rapid movement across accounts (layering) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `67ed5d2923` |
| 4 | AML-004 | Uso intenso de múltiplos instrumentos (card + transfer + cash-out) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `10290762dd` |
| 5 | AML-005 | Screening sanções: match forte (nome, DOB, país) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `04ab125284` |
| 6 | AML-006 | Structuring para evitar reporte (CTR) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `b52a9260a0` |
| 7 | AML-007 | Ransomware payment patterns (to high-risk CVC address) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `3324c8366c` |
| 8 | AML-008 | Conta potencial mule: muitos pagadores + retiradas/saídas rápidas | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `2e215f9ead` |
| 9 | AML-009 | Inconsistência de perfil: estudante/baixa renda com volume alto | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `81302d52c3` |
| 10 | AML-010 | Transações com finalidade incoerente/descrição suspeita | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `8b183dbf55` |
| 11 | ATO-001 | Credential stuffing (muitas falhas de login por IP/ASN) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `d907104b44` |
| 12 | ATO-002 | Spray attack (1 tentativa por usuário, muitos usuários no mesmo IP) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `a17a5e5f79` |
| 13 | ATO-003 | Login de novo device + nova geolocalização + sessão curta | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `b34f34e57e` |
| 14 | ATO-004 | Reset de senha seguido de alteração de email/telefone | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `056f5844c3` |
| 15 | ATO-005 | Alteração de dados + adição de beneficiário + transferência alta | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `17c0580a37` |
| 16 | ATO-006 | MFA fatigue (múltiplos prompts negados + 1 aceito) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `6db4ed1a94` |
| 17 | ATO-007 | Sessão anômala (mudança de device no meio da sessão) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `0a5f86664b` |
| 18 | ATO-008 | Login de datacenter/proxy + ação financeira | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `b4682fe801` |
| 19 | ATO-009 | Conta inativa reativada + transação imediata | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `7ff9623f7f` |
| 20 | ATO-010 | Tentativa de acessar conta com múltiplos user_agents incompatíveis | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `f927da5b9c` |
| 21 | BOT-001 | Criação de conta em massa por IP/device | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `74f131a8fe` |
| 22 | BOT-002 | Checkout/payment attempts em alta frequência (script) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `3a4c017603` |
| 23 | BOT-003 | Uso de user-agent impossível (inconsistência OS/browser) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `3d60bed37d` |
| 24 | BOT-004 | IP reputação ruim (spam/botnet) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `639ec2cb3b` |
| 25 | BOT-005 | Tentativas de cupom/promo em alta (promo abuse) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `807cde841f` |
| 26 | BOT-006 | Ataque de scraping a endpoints sensíveis | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `495b4c3280` |
| 27 | BOT-007 | Troca rápida de fingerprints (rotating devices) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `05b3c532b6` |
| 28 | BOT-008 | Múltiplos pagamentos com cartões diferentes em sequência (carding bot) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `167744dc08` |
| 29 | CT-001 | Card testing por PAN (muitas tentativas pequenas + alto declínio) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `4af80efc3a` |
| 30 | CT-002 | CVV brute-force (mesmo PAN, múltiplos CVV/expiração) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `7ad1a70eef` |
| 31 | CT-003 | Card enumeration por BIN + merchant (muitos PANs diferentes no mesmo merchant) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `707ebd90d8` |
| 32 | CT-004 | CNP: 1º uso do cartão + país do IP ≠ país do billing | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `5f3ea62704` |
| 33 | CT-005 | CNP: endereço de entrega de alto risco (drop / frete expresso) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `2d08d4c254` |
| 34 | CT-006 | CNP: mismatch forte (nome do titular vs nome do recebedor/entrega) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `6b9cf18c11` |
| 35 | CT-007 | Pico de chargeback/refund por merchant | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `d391f99a3d` |
| 36 | CT-008 | Velocidade por device (múltiplas compras em contas/cartões distintos) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `70013002e6` |
| 37 | CT-009 | 3DS falhou e tentativa imediata sem 3DS (fallback) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `f7dfc2f789` |
| 38 | CT-010 | MCC de alto risco + primeira transação + ticket alto | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `4c12837172` |
| 39 | KYC-001 | Cadastro: múltiplas contas compartilhando o mesmo device/telefone | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `7a3fdc0639` |
| 40 | KYC-002 | Cadastro: email descartável/temporário | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `20e5e49561` |
| 41 | KYC-003 | Cadastro: VOIP/virtual phone + alto risco | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `6c10cf3d88` |
| 42 | KYC-004 | Cadastro: inconsistência de nome/documento/idade | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `26c9130941` |
| 43 | KYC-005 | Cadastro + 1ª transação em < 10 min (padrão de cash-out) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `19648fd0a7` |
| 44 | KYC-006 | Sinais de deepfake/selfie anômalo (processo remoto) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `28ab266c0b` |
| 45 | KYC-007 | Endereço de alto risco (redirecionador / PO Box / locker) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `fb11e84e0f` |
| 46 | KYC-008 | Vários perfis diferentes usando o mesmo cartão/conta de funding | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `30feddc608` |
| 47 | KYC-009 | Fraude em empréstimo: renda alta + dados frágeis + device compartilhado | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `191af8bbf4` |
| 48 | KYC-010 | Tentativa de evitar verificação (abandona quando pede MFA/KYC) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `f575c4610f` |
| 49 | TRF-001 | Novo beneficiário + primeira transferência alta (APP fraud) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `7c548882c8` |
| 50 | TRF-002 | Múltiplas tentativas para o mesmo beneficiário com valores ajustados | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `f1617ab52c` |
| 51 | TRF-003 | Sequência típica de mule: entrada → saída rápida | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `6840ea035b` |
| 52 | TRF-004 | Estruturação (smurfing): múltiplas saídas abaixo do limite | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `b4b2c670e5` |
| 53 | TRF-005 | Transferência para jurisdição de alto risco (sanções/AML) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `b792d1e1bc` |
| 54 | TRF-006 | Mudança de chave PIX/beneficiário recente + transferência | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `1568ad1fb5` |
| 55 | TRF-007 | Pagamento QR code: QR reutilizado em múltiplos pagadores (golpe) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `a225502a09` |
| 56 | TRF-008 | ACH/transferência: alteração de dados bancários do recebedor + pagamento | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `7c9396ba73` |
| 57 | TRF-009 | Outbounds para cripto-exchange (VA) com padrão de cash-out | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `c0b491ddf6` |
| 58 | TRF-010 | Múltiplos pagamentos para beneficiários distintos em curto período | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `90246a15ed` |
| 59 | TRF-011 | NACHA/ACH: anomalia de velocidade no originador (créditos) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `8dc45a2506` |
| 60 | TRF-012 | NACHA/ACH: RDFI recebendo créditos suspeitos (padrão mule) | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `749ea5c1ea` |
| 61 | X-001 | Regra adicional (parametrizável) #1: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `daa1b4228d` |
| 62 | X-002 | Regra adicional (parametrizável) #2: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `d174942788` |
| 63 | X-003 | Regra adicional (parametrizável) #3: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `bde2820644` |
| 64 | X-004 | Regra adicional (parametrizável) #4: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `d448590a8a` |
| 65 | X-005 | Regra adicional (parametrizável) #5: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `e6b2c438fb` |
| 66 | X-006 | Regra adicional (parametrizável) #6: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `01ffdde281` |
| 67 | X-007 | Regra adicional (parametrizável) #7: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `3bb4ba76c3` |
| 68 | X-008 | Regra adicional (parametrizável) #8: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `1d18d5e484` |
| 69 | X-009 | Regra adicional (parametrizável) #9: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `35a700a526` |
| 70 | X-010 | Regra adicional (parametrizável) #10: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `7a1f76bafe` |
| 71 | X-011 | Regra adicional (parametrizável) #11: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `b1cdb6c2cd` |
| 72 | X-012 | Regra adicional (parametrizável) #12: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `7c0f110413` |
| 73 | X-013 | Regra adicional (parametrizável) #13: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `bbd3f9152e` |
| 74 | X-014 | Regra adicional (parametrizável) #14: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `37b95a1efe` |
| 75 | X-015 | Regra adicional (parametrizável) #15: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `3b98a654b9` |
| 76 | X-016 | Regra adicional (parametrizável) #16: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `6ee4a552b6` |
| 77 | X-017 | Regra adicional (parametrizável) #17: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `98d68758c7` |
| 78 | X-018 | Regra adicional (parametrizável) #18: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `6c4406fa27` |
| 79 | X-019 | Regra adicional (parametrizável) #19: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `1ac4efa79b` |
| 80 | X-020 | Regra adicional (parametrizável) #20: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `b63e0fa64d` |
| 81 | X-021 | Regra adicional (parametrizável) #21: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `2aeca40947` |
| 82 | X-022 | Regra adicional (parametrizável) #22: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `dc458f1e06` |
| 83 | X-023 | Regra adicional (parametrizável) #23: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `f5fbfde1bf` |
| 84 | X-024 | Regra adicional (parametrizável) #24: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `005925e569` |
| 85 | X-025 | Regra adicional (parametrizável) #25: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `27a3f7a29a` |
| 86 | X-026 | Regra adicional (parametrizável) #26: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `b9b5f14673` |
| 87 | X-027 | Regra adicional (parametrizável) #27: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `d19f973689` |
| 88 | X-028 | Regra adicional (parametrizável) #28: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `e32ccc362b` |
| 89 | X-029 | Regra adicional (parametrizável) #29: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `c66758f6ad` |
| 90 | X-030 | Regra adicional (parametrizável) #30: combinação de anomalias multi-sinal | RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | `2209eacf79` |
| 91 | R-CT-01 | High-speed card testing (legado) | ANALISE_TEXTO_ANTERIOR | `3404364597` |
| 92 | R-SKM-01 | Digital skimming / Magecart (legado) | ANALISE_TEXTO_ANTERIOR | `b437d5b224` |
| 93 | R-MULE-01 | Entrada nova fonte → cash-out rápido (legado) | ANALISE_TEXTO_ANTERIOR | `092adc21e6` |

## Regras (conteúdo completo)

> **Nota:** quando duas regras são **duplicatas exatas** (mesmo texto), o conteúdo aparece apenas na primeira ocorrência do mesmo `Hash`.

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 5f0ee893ac -->
### AML-001 — High-risk country/jurisdiction (FATF list) + transações recorrentes
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 90  |  **Prioridade:** 95
- **Condições (pseudo):**
  - counterparty_country in FATF_HIGH_RISK
  - count(transactions, account_id, 30d) >= 3
- **Campos/Counters necessários:** counterparty_country, lists:FATF_HIGH_RISK, account_id, counters_30d
- **Notas/Exceções:** Manter lista FATF atualizada e parametrizável (alto risco / sob monitoramento).
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: c09400a572 -->
### AML-002 — TBML proxy: pagamentos comerciais incoerentes (valor x perfil)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 85
- **Condições (pseudo):**
  - merchant_category=='trade' OR wire_purpose in TRADE_PURPOSE_CODES
  - invoice_value >> historical_baseline OR repeated_round_amounts==true
- **Campos/Counters necessários:** merchant_category, wire_purpose, invoice_value, customer_baseline
- **Notas/Exceções:** TBML é difícil sem dados de comércio; usar heurísticas de outliers determinísticos.
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 67ed5d2923 -->
### AML-003 — Rapid movement across accounts (layering)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 92
- **Condições (pseudo):**
  - inbound_from_new_counterparty==true
  - outbound_to_new_counterparty within 24h
  - amount_similarity(inbound,outbound) >= 90%
- **Campos/Counters necessários:** inbound/outbound legs, counterparty_history, timestamps
- **Notas/Exceções:** Sinal clássico de layering (lavagem).
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 10290762dd -->
### AML-004 — Uso intenso de múltiplos instrumentos (card + transfer + cash-out)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 82
- **Condições (pseudo):**
  - count(distinct(channel), account_id, 7d) >= 3
  - sum(outbound,7d) >= threshold
- **Campos/Counters necessários:** channel, account_id, amount, counters_7d
- **Notas/Exceções:** Combinações de canais podem indicar mule/ATO.
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 04ab125284 -->
### AML-005 — Screening sanções: match forte (nome, DOB, país)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 100  |  **Prioridade:** 100
- **Condições (pseudo):**
  - sanctions_match_score >= 0.95
- **Campos/Counters necessários:** name, dob, country, sanctions_engine_score
- **Notas/Exceções:** Bloqueio imediato + procedimento compliance.
- **Fontes (do anexo):**
  - https://sanctionssearch.ofac.treas.gov

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: b52a9260a0 -->
### AML-006 — Structuring para evitar reporte (CTR)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 92  |  **Prioridade:** 95
- **Condições (pseudo):**
  - count(cash_like_tx where amount in [threshold-ε], account_id, 7d) >= 3
- **Campos/Counters necessários:** amount, account_id, threshold_config, counters_7d
- **Notas/Exceções:** Indicador citado em materiais FinCEN (structuring).
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 3324c8366c -->
### AML-007 — Ransomware payment patterns (to high-risk CVC address)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 92
- **Condições (pseudo):**
  - counterparty_type=='crypto'
  - address_in_ransomware_list==true OR mixer_interaction==true
- **Campos/Counters necessários:** crypto_address, lists:ransomware, lists:mixers
- **Notas/Exceções:** Aplicar monitoramento VA e listas de risco.
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 2e215f9ead -->
### AML-008 — Conta potencial mule: muitos pagadores + retiradas/saídas rápidas
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 90  |  **Prioridade:** 94
- **Condições (pseudo):**
  - count(distinct(payer_id), account_id, 7d) >= 10
  - outbound_count(7d) >= 10
  - avg_balance_low==true
- **Campos/Counters necessários:** payer_id, account_id, balances, outbounds, counters_7d
- **Notas/Exceções:** Mule clássico: alto throughput e baixo saldo médio.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 81302d52c3 -->
### AML-009 — Inconsistência de perfil: estudante/baixa renda com volume alto
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 70  |  **Prioridade:** 70
- **Condições (pseudo):**
  - declared_occupation in LOW_INCOME_OCCUPATIONS
  - monthly_volume >= 5x declared_income
- **Campos/Counters necessários:** declared_occupation, declared_income, monthly_volume
- **Notas/Exceções:** Cuidado com falso positivo; usar como gatilho de revisão.
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 8b183dbf55 -->
### AML-010 — Transações com finalidade incoerente/descrição suspeita
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 65  |  **Prioridade:** 60
- **Condições (pseudo):**
  - purpose_text matches /(gift|urgent|investment|crypto|fee|release)/i
  - counterparty_new==true
- **Campos/Counters necessários:** purpose_text, counterparty_history
- **Notas/Exceções:** APP fraud e golpes usam descrições padronizadas. Ajustar por idioma/canal.

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: d907104b44 -->
### ATO-001 — Credential stuffing (muitas falhas de login por IP/ASN)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 95
- **Condições (pseudo):**
  - count(login_fail, ip, 5m) >= 50 OR count(distinct(username), ip, 5m) >= 30
  - user_agent_entropy(ip, 5m) low OR headless_signals==true
- **Campos/Counters necessários:** ip, asn, username, login_result, user_agent, timestamp, counters_5m
- **Notas/Exceções:** Ativar CAPTCHA/rate-limit + bloquear ASN/datacenter.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: a17a5e5f79 -->
### ATO-002 — Spray attack (1 tentativa por usuário, muitos usuários no mesmo IP)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(distinct(username), ip, 10m) >= 100
  - count(login_fail, ip, 10m) >= 90
- **Campos/Counters necessários:** ip, username, login_result, timestamp
- **Notas/Exceções:** Padrão de password-spray. Mitigar com throttling por IP + por username.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: b34f34e57e -->
### ATO-003 — Login de novo device + nova geolocalização + sessão curta
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 75  |  **Prioridade:** 80
- **Condições (pseudo):**
  - is_new_device(account_id, device_id)==true
  - geo_distance_km(prev_login, current_login) >= 500
  - time_since_prev_login <= 2h
- **Campos/Counters necessários:** account_id, device_id, ip_geo, timestamp, device_history
- **Notas/Exceções:** Exceção: viagens (sinal 'travel_mode' ou whitelist do cliente).
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 056f5844c3 -->
### ATO-004 — Reset de senha seguido de alteração de email/telefone
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 95
- **Condições (pseudo):**
  - password_reset==true
  - (email_change==true OR phone_change==true) within 24h
- **Campos/Counters necessários:** account_id, events_audit_log, timestamp
- **Notas/Exceções:** Sequência típica de takeover. Congelar payouts/transferências por 24–48h.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 17c0580a37 -->
### ATO-005 — Alteração de dados + adição de beneficiário + transferência alta
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 95  |  **Prioridade:** 99
- **Condições (pseudo):**
  - (email_change OR phone_change OR address_change) within 48h
  - new_beneficiary_added==true within 48h
  - transfer_amount >= p95(customer_transfers) OR transfer_amount >= 1000
- **Campos/Counters necessários:** account_id, beneficiary_id, transfer_amount, audit_events, customer_profile
- **Notas/Exceções:** Regra de 'cooldown' pós-mudança crítica.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 6db4ed1a94 -->
### ATO-006 — MFA fatigue (múltiplos prompts negados + 1 aceito)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 82  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(mfa_prompt_denied, account_id, 10m) >= 3
  - mfa_prompt_accepted within 10m
- **Campos/Counters necessários:** account_id, mfa_events, timestamp
- **Notas/Exceções:** Forçar reautenticação forte + confirmar canal alternativo.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 0a5f86664b -->
### ATO-007 — Sessão anômala (mudança de device no meio da sessão)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 90  |  **Prioridade:** 95
- **Condições (pseudo):**
  - session_id same
  - device_id changes OR ip changes ASN
  - high_risk_action within session
- **Campos/Counters necessários:** session_id, device_id, ip, asn, event_stream
- **Notas/Exceções:** Troca abrupta sugere hijack/proxy. Encerrar sessão e exigir login.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: b4682fe801 -->
### ATO-008 — Login de datacenter/proxy + ação financeira
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 85
- **Condições (pseudo):**
  - ip_is_datacenter==true OR ip_is_proxy==true
  - action in {'payout','transfer','add_card','change_credentials'}
- **Campos/Counters necessários:** ip_reputation, action, timestamp
- **Notas/Exceções:** Manter lista de ASN/datacenters e proxies conhecidos.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 7ff9623f7f -->
### ATO-009 — Conta inativa reativada + transação imediata
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 70  |  **Prioridade:** 75
- **Condições (pseudo):**
  - days_since_last_login >= 90
  - login_success==true
  - first_financial_action within 30m
- **Campos/Counters necessários:** account_id, last_login, event_times
- **Notas/Exceções:** Exigir step-up antes de ação sensível.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: f927da5b9c -->
### ATO-010 — Tentativa de acessar conta com múltiplos user_agents incompatíveis
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 72  |  **Prioridade:** 78
- **Condições (pseudo):**
  - count(distinct(user_agent_family), account_id, 1h) >= 4
  - login_fail_rate(account_id, 1h) >= 80%
- **Campos/Counters necessários:** account_id, user_agent, login_result, timestamp
- **Notas/Exceções:** Útil para detectar bots alternando UA.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 74f131a8fe -->
### BOT-001 — Criação de conta em massa por IP/device
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 85  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(signups, ip, 10m) >= 20 OR count(signups, device_id, 1h) >= 10
- **Campos/Counters necessários:** ip, device_id, signup_events, counters
- **Notas/Exceções:** Aplicar rate-limit + bloqueio temporário + CAPTCHA.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 3a4c017603 -->
### BOT-002 — Checkout/payment attempts em alta frequência (script)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 85
- **Condições (pseudo):**
  - count(payment_attempts, device_id, 5m) >= 30
  - avg_interaction_time < human_threshold
- **Campos/Counters necessários:** device_id, event_timestamps, front_end_metrics
- **Notas/Exceções:** Sinal de automação; combinar com headless/browser signals.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 3d60bed37d -->
### BOT-003 — Uso de user-agent impossível (inconsistência OS/browser)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 65  |  **Prioridade:** 60
- **Condições (pseudo):**
  - ua_inconsistent==true OR tls_fingerprint_mismatch==true
- **Campos/Counters necessários:** user_agent, tls_fp, device_fp
- **Notas/Exceções:** Baixa severidade isolada; usar como multiplicador.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 639ec2cb3b -->
### BOT-004 — IP reputação ruim (spam/botnet)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 70  |  **Prioridade:** 70
- **Condições (pseudo):**
  - ip_reputation in {'malicious','botnet','tor_exit'}
- **Campos/Counters necessários:** ip, ip_reputation_provider
- **Notas/Exceções:** Tor pode ser legítimo; tratar por risco do produto.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 807cde841f -->
### BOT-005 — Tentativas de cupom/promo em alta (promo abuse)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 75  |  **Prioridade:** 75
- **Condições (pseudo):**
  - count(coupon_apply_fail, account_id, 10m) >= 10 OR count(distinct(coupon_code), device_id, 30m) >= 20
- **Campos/Counters necessários:** coupon_code, account_id, device_id, counters
- **Notas/Exceções:** Fraude de cupom pode anteceder fraude de pagamento.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 495b4c3280 -->
### BOT-006 — Ataque de scraping a endpoints sensíveis
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 78  |  **Prioridade:** 80
- **Condições (pseudo):**
  - count(api_calls, ip, 1m) >= rate_limit_hard
  - endpoint in SENSITIVE_ENDPOINTS
- **Campos/Counters necessários:** ip, endpoint, api_call_logs, lists:SENSITIVE_ENDPOINTS
- **Notas/Exceções:** Aplicar WAF/ban por IP + token binding.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 05b3c532b6 -->
### BOT-007 — Troca rápida de fingerprints (rotating devices)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 72  |  **Prioridade:** 75
- **Condições (pseudo):**
  - count(distinct(device_fp), ip, 30m) >= 20
- **Campos/Counters necessários:** ip, device_fp, counters_30m
- **Notas/Exceções:** Rotação de fingerprints é comum em farms.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 167744dc08 -->
### BOT-008 — Múltiplos pagamentos com cartões diferentes em sequência (carding bot)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 92  |  **Prioridade:** 95
- **Condições (pseudo):**
  - count(distinct(pan_hash), account_id, 15m) >= 6
  - decline_rate(account_id, 15m) >= 70%
- **Campos/Counters necessários:** account_id, pan_hash, auth_result, counters_15m
- **Notas/Exceções:** Fortíssimo indicador de carding.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 4af80efc3a -->
### CT-001 — Card testing por PAN (muitas tentativas pequenas + alto declínio)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 95
- **Condições (pseudo):**
  - count(auth, pan_hash, 5m) >= 20
  - count(decline, pan_hash, 5m) >= 15
  - avg(amount, pan_hash, 5m) <= 15
- **Campos/Counters necessários:** pan_hash, amount, auth_result, merchant_id, timestamp, counter_by_pan_5m
- **Notas/Exceções:** Exceção: merchant allowlist (recorrência legítima) + contas antigas (idade > 180d).
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 7ad1a70eef -->
### CT-002 — CVV brute-force (mesmo PAN, múltiplos CVV/expiração)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 92  |  **Prioridade:** 98
- **Condições (pseudo):**
  - count(distinct(cvv), pan_hash, 10m) >= 5 OR count(distinct(expiry), pan_hash, 10m) >= 3
  - count(decline, pan_hash, 10m) >= 5
- **Campos/Counters necessários:** pan_hash, cvv_hash?, expiry, auth_result, timestamp, counter_distinct_by_pan_10m
- **Notas/Exceções:** Se não logar CVV, use 'reason_code' de CVV mismatch + repetição de tentativas.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 707ebd90d8 -->
### CT-003 — Card enumeration por BIN + merchant (muitos PANs diferentes no mesmo merchant)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(distinct(pan_hash), merchant_id, 15m) >= 50
  - decline_rate(merchant_id, 15m) >= 80%
- **Campos/Counters necessários:** merchant_id, pan_hash, auth_result, timestamp, counter_by_merchant_15m
- **Notas/Exceções:** Indicador forte de bot/ataque a formulário. Acionar WAF/rate-limit.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 5f3ea62704 -->
### CT-004 — CNP: 1º uso do cartão + país do IP ≠ país do billing
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 70  |  **Prioridade:** 75
- **Condições (pseudo):**
  - is_first_seen(pan_hash)==true
  - geo_country(ip)!=billing_country
  - amount >= p75(amount_by_pan) OR amount >= 200
- **Campos/Counters necessários:** pan_hash, ip, billing_country, amount, timestamp, first_seen_store
- **Notas/Exceções:** Se 3DS disponível: forçar 3DS challenge.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 2d08d4c254 -->
### CT-005 — CNP: endereço de entrega de alto risco (drop / frete expresso)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 75  |  **Prioridade:** 80
- **Condições (pseudo):**
  - shipping_is_forwarder==true OR address_in_drop_list==true
  - delivery_speed in {'same_day','next_day'}
  - amount >= 150
- **Campos/Counters necessários:** shipping_address, delivery_speed, amount, drop_list
- **Notas/Exceções:** Manter lista de redirecionadores conhecidos e PO Box/locker.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 6b9cf18c11 -->
### CT-006 — CNP: mismatch forte (nome do titular vs nome do recebedor/entrega)
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 68  |  **Prioridade:** 70
- **Condições (pseudo):**
  - levenshtein(cardholder_name, shipping_name) > threshold
  - account_age_days < 30
- **Campos/Counters necessários:** cardholder_name, shipping_name, account_age_days
- **Notas/Exceções:** Exceção: presente (gift) com marcação explícita + histórico bom.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: d391f99a3d -->
### CT-007 — Pico de chargeback/refund por merchant
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 85
- **Condições (pseudo):**
  - refund_rate(merchant_id, 7d) > 2x baseline OR chargeback_rate(merchant_id, 30d) > threshold_mcc
- **Campos/Counters necessários:** merchant_id, refund_flag, chargeback_flag, mcc, counters_7d_30d
- **Notas/Exceções:** Usar threshold por MCC/segmento e volume.

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 70013002e6 -->
### CT-008 — Velocidade por device (múltiplas compras em contas/cartões distintos)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 90  |  **Prioridade:** 96
- **Condições (pseudo):**
  - count(distinct(account_id), device_id, 1h) >= 5
  - count(distinct(pan_hash), device_id, 1h) >= 5
- **Campos/Counters necessários:** device_id, account_id, pan_hash, timestamp, counter_by_device_1h
- **Notas/Exceções:** Clássico de bot/farm. Acionar bloqueio por device + IP ASN.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: f7dfc2f789 -->
### CT-009 — 3DS falhou e tentativa imediata sem 3DS (fallback)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 88  |  **Prioridade:** 92
- **Condições (pseudo):**
  - prev(3ds_result)=='FAILED' within 10m
  - attempt_without_3ds==true
  - same_pan_hash==true
- **Campos/Counters necessários:** pan_hash, 3ds_result, eci, timestamp
- **Notas/Exceções:** Bloquear fallback de 3DS para transações de risco.
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 4c12837172 -->
### CT-010 — MCC de alto risco + primeira transação + ticket alto
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 72  |  **Prioridade:** 78
- **Condições (pseudo):**
  - mcc in HIGH_RISK_MCC
  - is_first_seen(account_id)==true
  - amount >= p90(amount_by_mcc)
- **Campos/Counters necessários:** mcc, amount, account_id, timestamp, lists:HIGH_RISK_MCC
- **Notas/Exceções:** HIGH_RISK_MCC deve ser parametrizável (ex.: gift cards, cripto, apostas).
- **Fontes (do anexo):**
  - https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
  - https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 7a3fdc0639 -->
### KYC-001 — Cadastro: múltiplas contas compartilhando o mesmo device/telefone
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(distinct(account_id), device_id, 7d) >= 3 OR count(distinct(account_id), phone, 30d) >= 3
- **Campos/Counters necessários:** device_id, phone, account_id, timestamp, counters_7d_30d
- **Notas/Exceções:** Pode indicar fazenda de contas ou família. Trate exceções (mesma residência).
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 20e5e49561 -->
### KYC-002 — Cadastro: email descartável/temporário
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 60  |  **Prioridade:** 60
- **Condições (pseudo):**
  - email_domain in DISPOSABLE_EMAIL_DOMAINS
- **Campos/Counters necessários:** email, lists:DISPOSABLE_EMAIL_DOMAINS
- **Notas/Exceções:** Exigir verificação extra + limitar limites iniciais.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 6c10cf3d88 -->
### KYC-003 — Cadastro: VOIP/virtual phone + alto risco
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 65  |  **Prioridade:** 65
- **Condições (pseudo):**
  - phone_type in {'voip','virtual'}
  - risk_score_context >= threshold
- **Campos/Counters necessários:** phone, phone_type, risk_context
- **Notas/Exceções:** VOIP não é fraude por si só; usar apenas em conjunto com outros sinais.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 26c9130941 -->
### KYC-004 — Cadastro: inconsistência de nome/documento/idade
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 85
- **Condições (pseudo):**
  - dob_age < 18 AND product_requires_adult==true OR doc_validation=='FAILED' OR name_mismatch==true
- **Campos/Counters necessários:** dob, doc_validation, name_fields, product_type
- **Notas/Exceções:** Reprovar quando doc inválido; segurar quando ambíguo.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 19648fd0a7 -->
### KYC-005 — Cadastro + 1ª transação em < 10 min (padrão de cash-out)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 82
- **Condições (pseudo):**
  - account_age_minutes < 10
  - first_funding_amount >= threshold_initial_funding
- **Campos/Counters necessários:** account_age_minutes, funding_amount, timestamp
- **Notas/Exceções:** Limitar operações até KYC concluído + cooldown inicial.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 28ab266c0b -->
### KYC-006 — Sinais de deepfake/selfie anômalo (processo remoto)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 90  |  **Prioridade:** 95
- **Condições (pseudo):**
  - liveness_check=='FAILED' OR selfie_similarity_low==true OR doc_image_artifacts==true
- **Campos/Counters necessários:** liveness_check, selfie_match, doc_forensics
- **Notas/Exceções:** Implementar lista de flags de forense (EXIF ausente, bordas artificiais, etc.).
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: fb11e84e0f -->
### KYC-007 — Endereço de alto risco (redirecionador / PO Box / locker)
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 70  |  **Prioridade:** 70
- **Condições (pseudo):**
  - address_type in {'po_box','locker','forwarder'} OR address_in_drop_list==true
- **Campos/Counters necessários:** address, drop_list, address_type
- **Notas/Exceções:** Pode ser legítimo (ex.: locker). Ajustar por produto.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 30feddc608 -->
### KYC-008 — Vários perfis diferentes usando o mesmo cartão/conta de funding
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(distinct(account_id), funding_pan_hash, 30d) >= 3 OR count(distinct(account_id), bank_account_hash, 30d) >= 3
- **Campos/Counters necessários:** funding_pan_hash, bank_account_hash, account_id, counters_30d
- **Notas/Exceções:** Sinal forte de synthetic/contas laranja.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 191af8bbf4 -->
### KYC-009 — Fraude em empréstimo: renda alta + dados frágeis + device compartilhado
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 85
- **Condições (pseudo):**
  - product_type=='loan'
  - declared_income >= p95(segment)
  - (thin_file==true OR kyc_confidence_low==true)
  - device_shared==true
- **Campos/Counters necessários:** product_type, declared_income, kyc_confidence, device_id, credit_bureau_flags?
- **Notas/Exceções:** Tratar como risco elevado e exigir documentação adicional.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: f575c4610f -->
### KYC-010 — Tentativa de evitar verificação (abandona quando pede MFA/KYC)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 75  |  **Prioridade:** 75
- **Condições (pseudo):**
  - user_abandons_on_step_up==true
  - retries_from_same_device_or_ip >= 3
- **Campos/Counters necessários:** funnel_events, device_id, ip, counters
- **Notas/Exceções:** Marcar device/IP como suspeito para cadastros futuros.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 7c548882c8 -->
### TRF-001 — Novo beneficiário + primeira transferência alta (APP fraud)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 95
- **Condições (pseudo):**
  - new_beneficiary==true
  - beneficiary_age_minutes < 60
  - amount >= max(500, p95(customer_transfers))
- **Campos/Counters necessários:** beneficiary_id, beneficiary_created_at, amount, customer_transfer_history
- **Notas/Exceções:** APP fraud costuma usar beneficiário recém-criado/alterado.

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: f1617ab52c -->
### TRF-002 — Múltiplas tentativas para o mesmo beneficiário com valores ajustados
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 85
- **Condições (pseudo):**
  - count(transfer_attempts, beneficiary_id, 30m) >= 5
  - values_show_staircase_pattern==true
- **Campos/Counters necessários:** beneficiary_id, amount, timestamp, counter_30m
- **Notas/Exceções:** Fraudador testa limites/antifraude (titration).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 6840ea035b -->
### TRF-003 — Sequência típica de mule: entrada → saída rápida
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 90  |  **Prioridade:** 96
- **Condições (pseudo):**
  - inbound_funds_received==true
  - outbound_transfer within 30m
  - outbound_to_new_beneficiary==true
- **Campos/Counters necessários:** inbound_event, outbound_event, timestamps, counter_flow
- **Notas/Exceções:** Travar saídas por janela curta para contas novas/alto risco.
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: b4b2c670e5 -->
### TRF-004 — Estruturação (smurfing): múltiplas saídas abaixo do limite
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 92
- **Condições (pseudo):**
  - count(outbound_transfers where amount in [limit-ε], account_id, 24h) >= 3
  - sum(outbound_transfers, 24h) >= 2*limit
- **Campos/Counters necessários:** account_id, amount, timestamp, limit_config, counters_24h
- **Notas/Exceções:** Aplicar por produto/canal e limites regulatórios internos.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: b792d1e1bc -->
### TRF-005 — Transferência para jurisdição de alto risco (sanções/AML)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 95  |  **Prioridade:** 99
- **Condições (pseudo):**
  - destination_country in HIGH_RISK_COUNTRIES OR counterparty_in_sanctions==true
- **Campos/Counters necessários:** destination_country, counterparty_id, lists:HIGH_RISK_COUNTRIES, sanctions_screening
- **Notas/Exceções:** Screening obrigatório (sanções).
- **Fontes (do anexo):**
  - https://sanctionssearch.ofac.treas.gov
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 1568ad1fb5 -->
### TRF-006 — Mudança de chave PIX/beneficiário recente + transferência
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 75  |  **Prioridade:** 85
- **Condições (pseudo):**
  - pix_key_changed within 24h OR beneficiary_details_changed within 24h
  - outbound_transfer_amount >= 300
- **Campos/Counters necessários:** pix_key, beneficiary_events, amount, timestamp
- **Notas/Exceções:** Regra de cooldown pós-alteração de chave/dados.

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: a225502a09 -->
### TRF-007 — Pagamento QR code: QR reutilizado em múltiplos pagadores (golpe)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 88
- **Condições (pseudo):**
  - payment_method=='qr'
  - count(distinct(payer_id), qr_payload_hash, 1h) >= 10
- **Campos/Counters necessários:** qr_payload_hash, payer_id, timestamp, counter_1h
- **Notas/Exceções:** QR reutilizado e divulgado em massa é comum em golpes.

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 7c9396ba73 -->
### TRF-008 — ACH/transferência: alteração de dados bancários do recebedor + pagamento
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 92
- **Condições (pseudo):**
  - beneficiary_bank_account_changed within 7d
  - payment_to_beneficiary==true
- **Campos/Counters necessários:** beneficiary_bank_hash, change_log, timestamp
- **Notas/Exceções:** Padrão de BEC (troca de dados de pagamento).
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: c0b491ddf6 -->
### TRF-009 — Outbounds para cripto-exchange (VA) com padrão de cash-out
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 82  |  **Prioridade:** 90
- **Condições (pseudo):**
  - counterparty_type=='crypto_exchange'
  - account_age_days < 30 OR recent_kyc_change==true
  - outbound_amount >= 500
- **Campos/Counters necessários:** counterparty_type, account_age_days, kyc_events, amount
- **Notas/Exceções:** Aplicar RBA para Virtual Assets; exigir step-up e limites iniciais.
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 90246a15ed -->
### TRF-010 — Múltiplos pagamentos para beneficiários distintos em curto período
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 75  |  **Prioridade:** 82
- **Condições (pseudo):**
  - count(distinct(beneficiary_id), account_id, 30m) >= 8
  - sum(amount, 30m) >= threshold
- **Campos/Counters necessários:** account_id, beneficiary_id, amount, counters_30m
- **Notas/Exceções:** Padrão de dispersão (mule/lavagem) ou conta comprometida.
- **Fontes (do anexo):**
  - https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
  - https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
  - https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 8dc45a2506 -->
### TRF-011 — NACHA/ACH: anomalia de velocidade no originador (créditos)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 88
- **Condições (pseudo):**
  - channel=='ACH'
  - count(credit_entries, originator_id, 1h) > 3x baseline
  - new_originator==true OR recent_bank_change==true
- **Campos/Counters necessários:** originator_id, channel, entry_type, timestamps, baseline_model (determinístico)
- **Notas/Exceções:** Regras Nacha 2026 exigem monitoramento para fraude (fase 1).
- **Fontes (do anexo):**
  - https://www.nacha.org/resources/fraud-and-risk-resources

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 749ea5c1ea -->
### TRF-012 — NACHA/ACH: RDFI recebendo créditos suspeitos (padrão mule)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 90
- **Condições (pseudo):**
  - channel=='ACH'
  - count(credits_received, account_id, 24h) >= 5
  - outbound_within_2h==true
- **Campos/Counters necessários:** channel, account_id, credits, outbounds, counters
- **Notas/Exceções:** Implementar processo risk-based p/ identificar créditos iniciados por fraude.
- **Fontes (do anexo):**
  - https://www.nacha.org/resources/fraud-and-risk-resources

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: daa1b4228d -->
### X-001 — Regra adicional (parametrizável) #1: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 61  |  **Prioridade:** 51
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: d174942788 -->
### X-002 — Regra adicional (parametrizável) #2: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 62  |  **Prioridade:** 52
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: bde2820644 -->
### X-003 — Regra adicional (parametrizável) #3: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 63  |  **Prioridade:** 53
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: d448590a8a -->
### X-004 — Regra adicional (parametrizável) #4: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 64  |  **Prioridade:** 54
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: e6b2c438fb -->
### X-005 — Regra adicional (parametrizável) #5: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 65  |  **Prioridade:** 55
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 01ffdde281 -->
### X-006 — Regra adicional (parametrizável) #6: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 66  |  **Prioridade:** 56
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 3bb4ba76c3 -->
### X-007 — Regra adicional (parametrizável) #7: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 67  |  **Prioridade:** 57
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 1d18d5e484 -->
### X-008 — Regra adicional (parametrizável) #8: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 68  |  **Prioridade:** 58
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 35a700a526 -->
### X-009 — Regra adicional (parametrizável) #9: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 69  |  **Prioridade:** 59
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 7a1f76bafe -->
### X-010 — Regra adicional (parametrizável) #10: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 70  |  **Prioridade:** 60
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: b1cdb6c2cd -->
### X-011 — Regra adicional (parametrizável) #11: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 71  |  **Prioridade:** 61
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 7c0f110413 -->
### X-012 — Regra adicional (parametrizável) #12: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 72  |  **Prioridade:** 62
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: bbd3f9152e -->
### X-013 — Regra adicional (parametrizável) #13: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 73  |  **Prioridade:** 63
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 37b95a1efe -->
### X-014 — Regra adicional (parametrizável) #14: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 74  |  **Prioridade:** 64
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 3b98a654b9 -->
### X-015 — Regra adicional (parametrizável) #15: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 75  |  **Prioridade:** 65
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 6ee4a552b6 -->
### X-016 — Regra adicional (parametrizável) #16: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 76  |  **Prioridade:** 66
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 98d68758c7 -->
### X-017 — Regra adicional (parametrizável) #17: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 77  |  **Prioridade:** 67
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 6c4406fa27 -->
### X-018 — Regra adicional (parametrizável) #18: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 68
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 1ac4efa79b -->
### X-019 — Regra adicional (parametrizável) #19: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 79  |  **Prioridade:** 69
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: b63e0fa64d -->
### X-020 — Regra adicional (parametrizável) #20: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 70
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 2aeca40947 -->
### X-021 — Regra adicional (parametrizável) #21: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 81  |  **Prioridade:** 71
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: dc458f1e06 -->
### X-022 — Regra adicional (parametrizável) #22: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 82  |  **Prioridade:** 72
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: f5fbfde1bf -->
### X-023 — Regra adicional (parametrizável) #23: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 83  |  **Prioridade:** 73
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 005925e569 -->
### X-024 — Regra adicional (parametrizável) #24: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 84  |  **Prioridade:** 74
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 27a3f7a29a -->
### X-025 — Regra adicional (parametrizável) #25: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 75
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: b9b5f14673 -->
### X-026 — Regra adicional (parametrizável) #26: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 86  |  **Prioridade:** 76
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: d19f973689 -->
### X-027 — Regra adicional (parametrizável) #27: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 87  |  **Prioridade:** 77
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: e32ccc362b -->
### X-028 — Regra adicional (parametrizável) #28: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 78
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: c66758f6ad -->
### X-029 — Regra adicional (parametrizável) #29: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 89  |  **Prioridade:** 79
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

<!-- ORIGEM: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md | HASH: 2209eacf79 -->
### X-030 — Regra adicional (parametrizável) #30: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 90  |  **Prioridade:** 80
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

## 5) Como minerar as demais ~4k URLs do anexo (pipeline recomendado)

Se você quiser **cobertura total**, o caminho mais eficiente é automatizar uma pipeline:

1. **Triage por domínio:** priorizar Tier 1 (reguladores/LEA/standards) → Tier 2 (bancos/consórcios/indústria) → Tier 3 (blogs).
2. **Extrator de padrões:** para cada URL, capturar: tipologia, sinais observáveis, entidade (conta/device/ip/merchant), janela temporal, ação sugerida.
3. **Normalização:** converter para um template único de regra (como o item 3).
4. **Deduplicação semântica:** agrupar regras equivalentes e manter a versão mais objetiva.
5. **Bateria de testes:** gerar casos (positivo/negativo) por regra e validar regressão.

<!-- ORIGEM: ANALISE_TEXTO_ANTERIOR | HASH: 3404364597 -->
### R-CT-01 — High-speed card testing (legado)
- **Ação/Decisão sugerida:** SUSPEITA_DE_FRAUDE + rate-limit/bloqueio temporário
- **Severidade (0–100):** 85  |  **Prioridade:** 95
- **Condições (pseudo):**
  - velocity.count_by_pan_5m >= 20
  - velocity.declines_by_pan_5m >= 15
  - avg(amount, pan, 5m) <= baixo
- **Campos/Counters necessários:** pan_hash, amount, auth_result, merchant_id, timestamp, counters por PAN/5m
- **Notas/Exceções:** merchant allowlist + janelas por MCC
- **Origem:** regra descrita na análise textual anterior (EPC/Recorded Future)

<!-- ORIGEM: ANALISE_TEXTO_ANTERIOR | HASH: b437d5b224 -->
### R-SKM-01 — Digital skimming / Magecart (legado)
- **Ação/Decisão sugerida:** BLOQUEAR ou STEP-UP (dependendo do produto)
- **Severidade (0–100):** 80  |  **Prioridade:** 85
- **Condições (pseudo):**
  - merchant_id ∈ LISTA_COMPROMETIDOS
- **Campos/Counters necessários:** merchant_id, lista dinâmica "comprometidos"
- **Notas/Exceções:** rever falsos positivos; lista precisa ser atualizada continuamente
- **Origem:** regra descrita na análise textual anterior (EPC)

<!-- ORIGEM: ANALISE_TEXTO_ANTERIOR | HASH: 092adc21e6 -->
### R-MULE-01 — Entrada nova fonte → cash-out rápido (legado)
- **Ação/Decisão sugerida:** SEGURAR/REVISÃO + cooldown
- **Severidade (0–100):** 90  |  **Prioridade:** 96
- **Condições (pseudo):**
  - inbound_from_new_source == true
  - outbound_cashout within 30m
- **Campos/Counters necessários:** eventos inbound/outbound, timestamps, "new_source" detector
- **Notas/Exceções:** tratar payroll/benefícios e padrões legítimos
- **Origem:** regra descrita na análise textual anterior (EPC/FinCEN)

## Relatório de duplicatas exatas (por Hash)

- Nenhuma duplicata exata detectada.
