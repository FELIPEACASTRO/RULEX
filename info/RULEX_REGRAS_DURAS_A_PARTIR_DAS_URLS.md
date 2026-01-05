# RULEX — Backlog de Regras Duras a partir do Anexo de URLs

**Data:** 2026-01-05  
**Objetivo:** transformar tipologias/alertas contidos nas URLs do anexo em um **catálogo prático de regras duras (determinísticas)** para o Rulex.

## 1) Auditoria das URLs do Anexo (sem redundância)

- **URLs encontradas (brutas, como aparecem no arquivo):** 5067
- **URLs canônicas únicas (normalizadas — remove tags tipo `,FinCEN`, normaliza barra final, etc.):** 4049
- **Links para PDF (dentro das URLs canônicas):** 377
- **Domínios governamentais (aprox. `.gov/.gov.uk/.gov.br` etc.):** 118

> Observação: o anexo contém várias linhas no formato `URL,AlgumaFonte` (ex.: `...,FinCEN`). Para evitar pseudo-duplicidade, a contagem “canônica” remove esse sufixo e normaliza a URL.

### 1.1 Top domínios no anexo (por quantidade de URLs canônicas)

| Domínio | Qtde |
|---|---:|
| github.com | 869 |
| arxiv.org | 727 |
| www.nber.org | 158 |
| www.kaggle.com | 148 |
| en.wikipedia.org | 129 |
| ieeexplore.ieee.org | 80 |
| link.springer.com | 57 |
| dl.acm.org | 51 |
| zenodo.org | 50 |
| archive.connect.h1.co | 38 |
| www.sciencedirect.com | 37 |
| www.openml.org | 34 |
| figshare.com | 31 |
| journals.plos.org | 24 |
| linkinghub.elsevier.com | 24 |
| www.mdpi.com | 23 |
| www.philadelphiafed.org | 19 |
| uir.unisa.ac.za | 17 |
| nva.sikt.no | 16 |
| nvlpubs.nist.gov | 16 |
| www.nature.com | 16 |
| archive.ics.uci.edu | 15 |
| www.adb.org | 15 |
| www.frontiersin.org | 15 |
| scikit-learn.org | 13 |
| www.fatf-gafi.org | 12 |
| dspace.mit.edu | 11 |
| www.bis.org | 11 |
| paperswithcode.com | 10 |
| springernature.figshare.com | 10 |
| proceedings.mlr.press | 9 |
| huggingface.co | 9 |
| tandf.figshare.com | 9 |
| www.ijcai.org | 9 |
| carleton.scholaris.ca | 8 |
| openreview.net | 8 |
| papers.ssrn.com | 8 |
| pdfs.semanticscholar.org | 8 |
| researchspace.ukzn.ac.za | 8 |
| towardsdatascience.com | 8 |

## 2) Fontes prioritárias (Tier 1) dentro do anexo

Estas fontes tendem a trazer **tipologias + red flags + controles** que viram regras duras diretamente (fraude, golpes APP, mule, AML, identidade, cyber).

- **FATF - Recomendações:** https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
- **FATF - Virtual Assets (RBA):** https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
- **FATF - TBML:** https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html
- **Europol - Payment Fraud Threat Landscape:** https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
- **Europol - IOCTA (página principal):** https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment
- **FinCEN - Advisories (índice):** https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets
- **NACHA - Fraud & Risk Resources:** https://www.nacha.org/resources/fraud-and-risk-resources
- **NIST - SP 800-63A:** https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf
- **PCI SSC (home):** https://www.pcisecuritystandards.org/
- **BIS - CPMI/BCBS:** https://www.bis.org/publ/bcbs128.pdf
- **FFIEC - BSA/AML InfoBase:** https://www.ffiec.gov/bsa_aml_infobase/pages_manual/manual_online.htm
- **OFAC - Sanctions Search:** https://sanctionssearch.ofac.treas.gov

### 2.1 Lista rápida (até 200 URLs Tier 1)

- https://data.europa.eu/en
- https://data.gov.uk/search?q=fraud
- https://ec.europa.eu/eurostat/data/database
- https://nvlpubs.nist.gov/nistpubs/CSWP/NIST.CSWP.01142020.pdf
- https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication500-157.pdf
- https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-124.pdf
- https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-14.pdf
- https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-144.pdf
- https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-25.pdf
- https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-48.pdf
- https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-60v2r1.pdf
- https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-98.pdf
- https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.1500-1r2.pdf
- https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.1800-17.pdf
- https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-12r1.pdf
- https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-163r1.pdf
- https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf
- https://nvlpubs.nist.gov/nistpubs/ai/NIST.AI.100-2e2023.pdf
- https://nvlpubs.nist.gov/nistpubs/ir/2022/NIST.IR.8403.pdf
- https://sdw.ecb.europa.eu
- https://www.bis.org/cpmi/publ/d137.pdf
- https://www.bis.org/cpmi/publ/d170.pdf
- https://www.bis.org/list/speeches/index.htm
- https://www.bis.org/publ/bcbs128.pdf
- https://www.bis.org/publ/bcbs195.pdf
- https://www.bis.org/publ/bcbs239.pdf
- https://www.bis.org/publ/bcbs270.pdf
- https://www.bis.org/publ/bcbs295.pdf
- https://www.bis.org/publ/bcbs316.pdf
- https://www.bis.org/publ/bcbs_wp35.pdf
- https://www.bis.org/statistics/index.htm
- https://www.cftc.gov/LawRegulation/Enforcement/index.htm
- https://www.cftc.gov/PressRoom/PressReleases
- https://www.cftc.gov/PressRoom/PressReleases/opapressreleases
- https://www.cftc.gov/PressRoom/SpeechesTestimony
- https://www.consumer.ftc.gov/articles/how-recognize-and-avoid-phishing-scams
- https://www.consumer.ftc.gov/features/scam-alerts
- https://www.consumerfinance.gov/data-research/consumer-complaints
- https://www.consumerfinance.gov/data-research/research-reports/consumer-voices-on-credit-reports-and-scores/full-report.pdf
- https://www.consumerfinance.gov/enforcement/actions
- https://www.eba.europa.eu/about-us/legal-framework/supervisory-convergence/breach-of-eu-law
- https://www.eba.europa.eu/regulation-and-policy/anti-money-laundering-and-countering-financing-terrorism
- https://www.eba.europa.eu/risk-and-data-analysis
- https://www.eba.europa.eu/sites/default/documents/files/document_library/Publications/Reports/2021/1001083/EBA%20report%20on%20SupTech.pdf
- https://www.eba.europa.eu/sites/default/documents/files/document_library/Publications/Reports/2022/1025155/Report%20on%20RegTech%20and%20SupTech%20and%20the%20use%20of%20innovative%20technologies.pdf
- https://www.eba.europa.eu/sites/default/documents/files/document_library/Risk%20Analysis/Report%20on%20Payment%20Fraud
- https://www.ecb.europa.eu/press/pr/activities/ara/html/index.en.html
- https://www.ecb.europa.eu/pub/cardfraud/html/index.en.html
- https://www.ecb.europa.eu/pub/pdf/scpops/ecb.op290~3f3e2b3e7f.en.pdf
- https://www.eiopa.europa.eu/publications/fraud-detection-and-prevention_en
- https://www.esma.europa.eu/press-news/esma-news
- https://www.esma.europa.eu/sites/default/files/library/esma_guidelines_market_abuse.pdf
- https://www.esma.europa.eu/sites/default/files/library/fraud-prevention-report.pdf
- https://www.europol.europa.eu/crime-areas-and-statistics/crime-areas/cybercrime
- https://www.europol.europa.eu/crime-areas-and-statistics/crime-areas/economic-crime
- https://www.europol.europa.eu/crime-areas-and-statistics/crime-areas/economic-crime/fraud
- https://www.europol.europa.eu/newsroom
- https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment
- https://www.europol.europa.eu/publications-events/publications/payment-fraud-threat-landscape
- https://www.fatf-gafi.org/en/home.html
- https://www.fatf-gafi.org/en/publications.html
- https://www.fatf-gafi.org/en/publications/Fatfgeneral/Fatf-fintech-regtech-forum.html
- https://www.fatf-gafi.org/en/publications/Fatfrecommendations/guidance-rba-virtual-assets.html
- https://www.fatf-gafi.org/en/publications/Fatfrecommendations/proliferation-financing.html
- https://www.fatf-gafi.org/en/publications/Fatfrecommendations/targeted-update-2021.html
- https://www.fatf-gafi.org/en/publications/Methodsandtrends/documents/trade-based-money-laundering.html
- https://www.fatf-gafi.org/media/fatf/documents/reports/Digital-Identity.pdf
- https://www.fatf-gafi.org/media/fatf/documents/reports/Professional-Money-Laundering.pdf
- https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html
- https://www.fatf-gafi.org/publications/methodsandtrends/documents/money-laundering-terrorist-financing-trends.html
- https://www.fatf-gafi.org/publications/mutualevaluations
- https://www.fbi.gov/file-repository/ic3-annual-report-2022.pdf
- https://www.fbi.gov/investigate/white-collar-crime
- https://www.fbi.gov/investigate/white-collar-crime/fraud
- https://www.fbi.gov/news/stories
- https://www.fbi.gov/scams-and-safety/common-frauds-and-scams
- https://www.ffiec.gov/bsa_aml_infobase/pages_manual/manual_online.htm
- https://www.ffiec.gov/cra/craflatfiles.htm
- https://www.fincen.gov/news-room/enforcement-actions
- https://www.fincen.gov/news-room/news-releases
- https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets
- https://www.fincen.gov/resources/data
- https://www.fincen.gov/sites/default/files/shared/508_Cryptocurrency_Typology_Report_FINAL_508.pdf
- https://www.fincen.gov/sites/default/files/shared/FinCEN_Advisory_Synthetic_Identity_Fraud.pdf
- https://www.fincen.gov/sites/default/files/shared/Financial_Trend_Analysis_COVID19_FINALa.pdf
- https://www.ftc.gov/enforcement/cases-proceedings
- https://www.ftc.gov/news-events/data-visualizations/data-spotlight
- https://www.ftc.gov/news-events/news/press-releases
- https://www.ftc.gov/reports/consumer-sentinel-network-data-book-2021
- https://www.ftc.gov/system/files/ftc_gov/pdf/CSN-Data-Book-2022.pdf
- https://www.interpol.int/en/Crimes/Financial-crime
- https://www.interpol.int/en/News-and-Events/News
- https://www.interpol.int/en/content/download/15234/file/Financial-Crime-Threat-Assessment.pdf
- https://www.nationalcrimeagency.gov.uk/news
- https://www.nist.gov/cyberframework
- https://www.nist.gov/system/files/documents/2021/08/04/fraud-detection-ai-ml.pdf
- https://www.occ.treas.gov/topics/supervision-and-examination/bank-operations/cybersecurity/index-cybersecurity.html
- https://www.pcisecuritystandards.org/
- https://www.sec.gov/about/reports-publications/special-studies/dera-wp-online-broker-fraud.pdf
- https://www.sec.gov/dera/data/financial-statement-data-sets.html
- https://www.sec.gov/edgar/searchedgar/companysearch.html
- https://www.sec.gov/litigation/admin.htm
- https://www.sec.gov/litigation/litreleases.htm
- https://www.sec.gov/litigation/litreleases.shtml
- https://www.sec.gov/news/pressreleases
- https://www.sfo.gov.uk/news
- https://www.ukfinance.org.uk/policy-and-guidance/reports-and-publications/fraud-report-2022
- https://www.ukfinance.org.uk/policy-and-guidance/reports-publications

## 3) Modelo mínimo sugerido de regra dura (para mapear no Rulex)

- **Entrada:** evento (cadastro/login/transação), contexto (cliente/conta), e listas/counters.
- **Saída:** decisão determinística (ex.: `APROVAR`, `STEP-UP`, `SEGURAR/REVISÃO`, `BLOQUEAR`).
- **Campos recomendados:** `account_id`, `customer_id`, `device_id`, `ip`, `asn`, `geo`, `amount`, `currency`, `merchant_id`, `mcc`, `channel`, `timestamp`, `beneficiary_id`, `kyc_flags`, `sanctions_screening`, `reason_codes`.
- **Counters padrão:** janelas 1m/5m/15m/1h/24h/7d/30d por entidade (`account`, `device`, `ip`, `merchant`, `beneficiary`, `pan_hash`).

## 4) Catálogo de Regras Duras (Backlog implementável)

> **Importante:** thresholds numéricos são **ponto de partida**. Ajuste por segmento, canal e baseline (de forma determinística), mantendo o motor 100% rule-based.

### CT — Cartões / CNP / Autorização

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

### CT-007 — Pico de chargeback/refund por merchant
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 85
- **Condições (pseudo):**
  - refund_rate(merchant_id, 7d) > 2x baseline OR chargeback_rate(merchant_id, 30d) > threshold_mcc
- **Campos/Counters necessários:** merchant_id, refund_flag, chargeback_flag, mcc, counters_7d_30d
- **Notas/Exceções:** Usar threshold por MCC/segmento e volume.

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

### ATO — Account Takeover / Login

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

### KYC — Onboarding / KYC / Identidade

### KYC-001 — Cadastro: múltiplas contas compartilhando o mesmo device/telefone
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(distinct(account_id), device_id, 7d) >= 3 OR count(distinct(account_id), phone, 30d) >= 3
- **Campos/Counters necessários:** device_id, phone, account_id, timestamp, counters_7d_30d
- **Notas/Exceções:** Pode indicar fazenda de contas ou família. Trate exceções (mesma residência).
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

### KYC-002 — Cadastro: email descartável/temporário
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 60  |  **Prioridade:** 60
- **Condições (pseudo):**
  - email_domain in DISPOSABLE_EMAIL_DOMAINS
- **Campos/Counters necessários:** email, lists:DISPOSABLE_EMAIL_DOMAINS
- **Notas/Exceções:** Exigir verificação extra + limitar limites iniciais.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

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

### KYC-004 — Cadastro: inconsistência de nome/documento/idade
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 85
- **Condições (pseudo):**
  - dob_age < 18 AND product_requires_adult==true OR doc_validation=='FAILED' OR name_mismatch==true
- **Campos/Counters necessários:** dob, doc_validation, name_fields, product_type
- **Notas/Exceções:** Reprovar quando doc inválido; segurar quando ambíguo.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

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

### KYC-006 — Sinais de deepfake/selfie anômalo (processo remoto)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 90  |  **Prioridade:** 95
- **Condições (pseudo):**
  - liveness_check=='FAILED' OR selfie_similarity_low==true OR doc_image_artifacts==true
- **Campos/Counters necessários:** liveness_check, selfie_match, doc_forensics
- **Notas/Exceções:** Implementar lista de flags de forense (EXIF ausente, bordas artificiais, etc.).
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

### KYC-007 — Endereço de alto risco (redirecionador / PO Box / locker)
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 70  |  **Prioridade:** 70
- **Condições (pseudo):**
  - address_type in {'po_box','locker','forwarder'} OR address_in_drop_list==true
- **Campos/Counters necessários:** address, drop_list, address_type
- **Notas/Exceções:** Pode ser legítimo (ex.: locker). Ajustar por produto.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

### KYC-008 — Vários perfis diferentes usando o mesmo cartão/conta de funding
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(distinct(account_id), funding_pan_hash, 30d) >= 3 OR count(distinct(account_id), bank_account_hash, 30d) >= 3
- **Campos/Counters necessários:** funding_pan_hash, bank_account_hash, account_id, counters_30d
- **Notas/Exceções:** Sinal forte de synthetic/contas laranja.
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

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

### TRF — Transferências / PIX / ACH / APP Fraud

### TRF-001 — Novo beneficiário + primeira transferência alta (APP fraud)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 95
- **Condições (pseudo):**
  - new_beneficiary==true
  - beneficiary_age_minutes < 60
  - amount >= max(500, p95(customer_transfers))
- **Campos/Counters necessários:** beneficiary_id, beneficiary_created_at, amount, customer_transfer_history
- **Notas/Exceções:** APP fraud costuma usar beneficiário recém-criado/alterado.

### TRF-002 — Múltiplas tentativas para o mesmo beneficiário com valores ajustados
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 85
- **Condições (pseudo):**
  - count(transfer_attempts, beneficiary_id, 30m) >= 5
  - values_show_staircase_pattern==true
- **Campos/Counters necessários:** beneficiary_id, amount, timestamp, counter_30m
- **Notas/Exceções:** Fraudador testa limites/antifraude (titration).

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

### TRF-006 — Mudança de chave PIX/beneficiário recente + transferência
- **Ação/Decisão sugerida:** STEP-UP (MFA/3DS/KYC extra)
- **Severidade (0–100):** 75  |  **Prioridade:** 85
- **Condições (pseudo):**
  - pix_key_changed within 24h OR beneficiary_details_changed within 24h
  - outbound_transfer_amount >= 300
- **Campos/Counters necessários:** pix_key, beneficiary_events, amount, timestamp
- **Notas/Exceções:** Regra de cooldown pós-alteração de chave/dados.

### TRF-007 — Pagamento QR code: QR reutilizado em múltiplos pagadores (golpe)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 88
- **Condições (pseudo):**
  - payment_method=='qr'
  - count(distinct(payer_id), qr_payload_hash, 1h) >= 10
- **Campos/Counters necessários:** qr_payload_hash, payer_id, timestamp, counter_1h
- **Notas/Exceções:** QR reutilizado e divulgado em massa é comum em golpes.

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

### AML — AML / Sanções / Tipologias

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

### AML-005 — Screening sanções: match forte (nome, DOB, país)
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 100  |  **Prioridade:** 100
- **Condições (pseudo):**
  - sanctions_match_score >= 0.95
- **Campos/Counters necessários:** name, dob, country, sanctions_engine_score
- **Notas/Exceções:** Bloqueio imediato + procedimento compliance.
- **Fontes (do anexo):**
  - https://sanctionssearch.ofac.treas.gov

### AML-006 — Structuring para evitar reporte (CTR)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 92  |  **Prioridade:** 95
- **Condições (pseudo):**
  - count(cash_like_tx where amount in [threshold-ε], account_id, 7d) >= 3
- **Campos/Counters necessários:** amount, account_id, threshold_config, counters_7d
- **Notas/Exceções:** Indicador citado em materiais FinCEN (structuring).
- **Fontes (do anexo):**
  - https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets

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

### AML-010 — Transações com finalidade incoerente/descrição suspeita
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 65  |  **Prioridade:** 60
- **Condições (pseudo):**
  - purpose_text matches /(gift|urgent|investment|crypto|fee|release)/i
  - counterparty_new==true
- **Campos/Counters necessários:** purpose_text, counterparty_history
- **Notas/Exceções:** APP fraud e golpes usam descrições padronizadas. Ajustar por idioma/canal.

### BOT — Bots / Abuso / Automação

### BOT-001 — Criação de conta em massa por IP/device
- **Ação/Decisão sugerida:** BLOQUEAR / FRAUDE
- **Severidade (0–100):** 85  |  **Prioridade:** 90
- **Condições (pseudo):**
  - count(signups, ip, 10m) >= 20 OR count(signups, device_id, 1h) >= 10
- **Campos/Counters necessários:** ip, device_id, signup_events, counters
- **Notas/Exceções:** Aplicar rate-limit + bloqueio temporário + CAPTCHA.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

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

### BOT-003 — Uso de user-agent impossível (inconsistência OS/browser)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 65  |  **Prioridade:** 60
- **Condições (pseudo):**
  - ua_inconsistent==true OR tls_fingerprint_mismatch==true
- **Campos/Counters necessários:** user_agent, tls_fp, device_fp
- **Notas/Exceções:** Baixa severidade isolada; usar como multiplicador.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

### BOT-004 — IP reputação ruim (spam/botnet)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 70  |  **Prioridade:** 70
- **Condições (pseudo):**
  - ip_reputation in {'malicious','botnet','tor_exit'}
- **Campos/Counters necessários:** ip, ip_reputation_provider
- **Notas/Exceções:** Tor pode ser legítimo; tratar por risco do produto.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

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

### BOT-007 — Troca rápida de fingerprints (rotating devices)
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 72  |  **Prioridade:** 75
- **Condições (pseudo):**
  - count(distinct(device_fp), ip, 30m) >= 20
- **Campos/Counters necessários:** ip, device_fp, counters_30m
- **Notas/Exceções:** Rotação de fingerprints é comum em farms.
- **Fontes (do anexo):**
  - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf

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

### X — Backlog extra (placeholders parametrizáveis)

### X-001 — Regra adicional (parametrizável) #1: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 61  |  **Prioridade:** 51
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-002 — Regra adicional (parametrizável) #2: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 62  |  **Prioridade:** 52
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-003 — Regra adicional (parametrizável) #3: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 63  |  **Prioridade:** 53
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-004 — Regra adicional (parametrizável) #4: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 64  |  **Prioridade:** 54
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-005 — Regra adicional (parametrizável) #5: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 65  |  **Prioridade:** 55
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-006 — Regra adicional (parametrizável) #6: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 66  |  **Prioridade:** 56
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-007 — Regra adicional (parametrizável) #7: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 67  |  **Prioridade:** 57
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-008 — Regra adicional (parametrizável) #8: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 68  |  **Prioridade:** 58
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-009 — Regra adicional (parametrizável) #9: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 69  |  **Prioridade:** 59
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-010 — Regra adicional (parametrizável) #10: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 70  |  **Prioridade:** 60
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-011 — Regra adicional (parametrizável) #11: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 71  |  **Prioridade:** 61
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-012 — Regra adicional (parametrizável) #12: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 72  |  **Prioridade:** 62
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-013 — Regra adicional (parametrizável) #13: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 73  |  **Prioridade:** 63
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-014 — Regra adicional (parametrizável) #14: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 74  |  **Prioridade:** 64
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-015 — Regra adicional (parametrizável) #15: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 75  |  **Prioridade:** 65
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-016 — Regra adicional (parametrizável) #16: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 76  |  **Prioridade:** 66
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-017 — Regra adicional (parametrizável) #17: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 77  |  **Prioridade:** 67
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-018 — Regra adicional (parametrizável) #18: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 78  |  **Prioridade:** 68
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-019 — Regra adicional (parametrizável) #19: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 79  |  **Prioridade:** 69
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-020 — Regra adicional (parametrizável) #20: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 80  |  **Prioridade:** 70
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-021 — Regra adicional (parametrizável) #21: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 81  |  **Prioridade:** 71
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-022 — Regra adicional (parametrizável) #22: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 82  |  **Prioridade:** 72
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-023 — Regra adicional (parametrizável) #23: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 83  |  **Prioridade:** 73
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-024 — Regra adicional (parametrizável) #24: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 84  |  **Prioridade:** 74
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-025 — Regra adicional (parametrizável) #25: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 85  |  **Prioridade:** 75
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-026 — Regra adicional (parametrizável) #26: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 86  |  **Prioridade:** 76
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 1.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-027 — Regra adicional (parametrizável) #27: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 87  |  **Prioridade:** 77
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-028 — Regra adicional (parametrizável) #28: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 88  |  **Prioridade:** 78
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 2.5
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

### X-029 — Regra adicional (parametrizável) #29: combinação de anomalias multi-sinal
- **Ação/Decisão sugerida:** SEGURAR / REVISÃO
- **Severidade (0–100):** 89  |  **Prioridade:** 79
- **Condições (pseudo):**
  - signal_A==true
  - signal_B==true
  - risk_multiplier >= 3.0
- **Campos/Counters necessários:** signal_A, signal_B, risk_multiplier, timestamp
- **Notas/Exceções:** Placeholder para plugar sinais específicos encontrados ao minerar as demais URLs (tier 2/3).

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

### 5.1 Prompt pronto (para agente tipo Devin/Claude) — extração 100% determinística

Copie e cole o prompt abaixo no seu agente (ele deve navegar nas URLs):

```text
Você é um auditor de prevenção a fraudes 100% RULE-BASED (sem ML).
Objetivo: para CADA URL fornecida, extrair tipologias e convertê-las em Regras Duras para o Rulex.

Regras de trabalho:
- Não invente fatos. Se a página não trouxer evidência, marque como 'SEM EVIDÊNCIA' e siga.
- Para cada regra, devolva: id, título, decisão, severidade (0-100), prioridade, condições (pseudo), campos/counters, exceções, URL fonte.
- Deduplicate: se duas URLs produzirem a mesma regra, mantenha 1 regra final e liste todas as fontes.
- Foque em sinais observáveis e implementáveis: velocidade, geolocalização, device, IP/ASN, histórico, listas, sequência de eventos, perfil do cliente.

Entregáveis:
1) Um arquivo Markdown com o catálogo final de regras duras.
2) Um dicionário de counters e listas necessárias.
3) Uma matriz de testes (casos positivos/negativos) por regra.
```

## 6) Apêndice — URLs canônicas únicas (opcional)

> Se você quiser, posso também gerar um arquivo separado só com todas as URLs canônicas únicas (4049) em lista limpa.
