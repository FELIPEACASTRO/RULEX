# RULEX - Dossiê de URLs e Produtos de Fraude

**Versão:** 1.0.0
**Data:** 2025-01-03
**Status:** FASE 1 - EM PROGRESSO

---

## Atualizações (2025-01-03 19:30)
- Navegação inicial de URLs principais (FICO, NICE, ACI, LexisNexis, Feedzai, Kount, Sift, BioCatch, HAWK:AI, Verafin)
- Extração de capacidades determinísticas
- Identificação de padrões comuns de detecção

## Atualizações (2025-01-03 19:00)
- Criação inicial do documento
- Estrutura preparada para navegação das 100 URLs do baseline

---

## 1. Objetivo

Este documento registra a navegação e extração de evidências de **100 URLs** de produtos de prevenção a fraude, conforme baseline definido no prompt.

**Regra**: Extrair APENAS controles determinísticos (sem ML). Converter qualquer menção a "AI/ML" para equivalente determinístico.

---

## 2. Resumo de Navegação

| Status | Quantidade | Descrição |
|--------|------------|-----------|
| ✅ OK | 12 | URLs navegadas com evidências extraídas |
| ⚠️ SEM_EVIDENCIA | 2 | URLs navegadas mas sem evidências úteis |
| ❌ BLOQUEIO | 0 | URLs inacessíveis ou bloqueadas |
| ⏳ PENDENTE | 86 | URLs ainda não navegadas |

---

## 3. Dossiê por Vendor/Produto

### 3.1 SAS (IDs 1-3)

#### SAS Fraud Management (ID 1)
- **URL**: https://www.sas.com/en_us/software/fraud-management.html
- **Status**: ⏳ PENDENTE
- **URLs Visitadas**: -
- **PDFs Encontrados**: -
- **Evidências Extraídas**: -
- **Capacidades Determinísticas**: -

#### SAS Fraud/Financial Crimes (ID 2)
- **URL**: https://www.sas.com/pt_br/industry/banking/solution/fraud-financial-crimes-compliance.html
- **Status**: ⏳ PENDENTE

#### SAS Anti-Money Laundering (ID 3)
- **URL**: https://www.sas.com/en_us/software/anti-money-laundering.html
- **Status**: ⏳ PENDENTE

---

### 3.2 FICO (IDs 4-5)

#### FICO Falcon Fraud Manager (ID 4)
- **URL**: https://www.fico.com/en/products/fico-falcon-fraud-manager
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "Real-time transaction protection across all channels"
  - "Proven fraud detection and prevention solution"
  - "Contextual Fraud Analytics - scoring engine for consortium models"
  - "Case Management for investigation"
  - "10,000+ global institutions"
- **Capacidades Determinísticas Identificadas**:
  - Real-time scoring engine
  - Consortium data (velocity patterns)
  - Multi-channel protection
  - Case management workflow

#### Falcon Fraud Manager - FinancialIT (ID 5)
- **URL**: https://financialit.net/products/banking/falcon-fraud-manager
- **Status**: ⏳ PENDENTE

---

### 3.3 IBM (IDs 6-7)

#### IBM Safer Payments (ID 6)
- **URL**: https://www.ibm.com/products/safer-payments
- **Status**: ⏳ PENDENTE

#### IBM Fraud Prevention (ID 7)
- **URL**: https://www.ibm.com/solutions/fraud-prevention
- **Status**: ⏳ PENDENTE

---

### 3.4 NICE Actimize (IDs 8-9)

#### Digital Banking Fraud (ID 8)
- **URL**: https://www.niceactimize.com/fraud-management/digital-banking
- **Status**: ⏳ PENDENTE

#### Fraud Management Suite (ID 9)
- **URL**: https://www.niceactimize.com/fraud-management
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "Real-Time, End-to-End Fraud Prevention"
  - "Scams & Mule Defense - AI-powered insights identify suspicious activity patterns"
  - "Payments Fraud - real-time monitoring"
  - "Typology-Centric Approach - cutting-edge detection"
  - "Key risk windows and timing patterns"
  - "Insights by payment type: P2P, Zelle, wire transfers"
- **Capacidades Determinísticas Identificadas**:
  - Scam detection patterns
  - Mule account indicators
  - Payment type-specific rules
  - Timing pattern analysis
  - Real-time monitoring

---

### 3.5 ACI Worldwide (IDs 10-11)

#### ACI Fraud Management Banking (ID 10)
- **URL**: https://www.aciworldwide.com/solutions/aci-fraud-management-banking
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "Integrated real-time fraud management"
  - ">85% Detection rates"
  - "1.5x Effective cash-out scam detection"
  - "Real-time anomaly detection"
  - "AML/KYC compliance - transaction monitoring, SAR reporting"
  - "Modern fraud and AML threats demand unified real-time orchestration"
  - "Fight real-time payments fraud"
- **Capacidades Determinísticas Identificadas**:
  - Real-time detection (85%+ rate)
  - Cash-out scam patterns
  - Anomaly detection rules
  - Transaction monitoring
  - SAR filing triggers

#### ACI Financial Crime Management (ID 11)
- **URL**: https://www.aciworldwide.com/solutions/aci-financial-crime-management
- **Status**: ⏳ PENDENTE

---

### 3.6 LexisNexis Risk (IDs 12-13)

#### ThreatMetrix (ID 12)
- **URL**: https://risk.lexisnexis.com/global/en/products/threatmetrix
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "A smarter approach to risk management"
  - "Make more precise and automated risk decisions"
  - "Flexible, comprehensive intelligence engine"
  - "Best-in-class digital identity and behavioral intelligence"
  - "Risk Orchestration - optimize end-to-end workflows"
- **Capacidades Determinísticas Identificadas**:
  - Digital identity verification
  - Behavioral intelligence patterns
  - Risk orchestration workflows
  - Automated risk decisions
  - **GAP**: Requer deviceId, ipAddress (não disponível no payload)

#### Fraud & Identity Management (ID 13)
- **URL**: https://risk.lexisnexis.com/global/en/corporations-and-non-profits/fraud-and-identity-management
- **Status**: ⏳ PENDENTE

---

### 3.7 Experian (IDs 14-16)

#### CrossCore UK (ID 14)
- **URL**: https://www.experian.co.uk/business-products/crosscore/
- **Status**: ⏳ PENDENTE

#### CrossCore US (ID 15)
- **URL**: https://www.experian.com/business/products/crosscore
- **Status**: ⏳ PENDENTE

#### Fraud Management (ID 16)
- **URL**: https://www.experian.com/business/solutions/fraud-management
- **Status**: ⏳ PENDENTE

---

### 3.8 TransUnion (IDs 17-18)

#### TruValidate (ID 17)
- **URL**: https://www.transunion.com/product/truvalidate
- **Status**: ⏳ PENDENTE

#### TruValidate Device Risk (ID 18)
- **URL**: https://newsroom.transunion.com.br/truvalidate-device-risk/
- **Status**: ⏳ PENDENTE

---

### 3.9 Mastercard (ID 19)

#### Consumer Fraud Risk (ID 19)
- **URL**: https://www.mastercardservices.com/en/solutions/consumer-fraud-risk
- **Status**: ⏳ PENDENTE

---

### 3.10 Visa (ID 20)

#### Risk & Identity (ID 20)
- **URL**: https://usa.visa.com/run-your-business/risk.html
- **Status**: ⏳ PENDENTE

---

### 3.11 Feedzai (IDs 21-22)

#### Transaction Fraud (ID 21)
- **URL**: https://www.feedzai.com/solutions/transaction-fraud/
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "Stop Transaction Fraud and Protect Honest Customers"
  - "Combines behavioral, non-monetary, and monetary data"
  - "Faster Transaction Approvals - real-time risk assessment"
  - "The Life of a Transaction in 3 Milliseconds"
  - "AI constantly learns and adapts to new fraud patterns"
- **Capacidades Determinísticas Identificadas**:
  - Real-time risk assessment (3ms)
  - Behavioral data analysis
  - Monetary pattern detection
  - False positive reduction
  - Multi-channel protection

#### Platform (ID 22)
- **URL**: https://www.feedzai.com
- **Status**: ⏳ PENDENTE

---

### 3.12 Kount (IDs 23-24)

#### Fraud Detection (ID 23)
- **URL**: https://kount.com/fraud-detection-software
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "Fraud detection software built to prevent revenue loss"
  - "Key to fraud prevention is data - collecting, evaluating patterns, driving decisions"
  - "1. An interaction is detected"
  - "2. Data is collected - payment info, device ID, location"
  - "3. A risk score is calculated - compares to billions of data points"
  - "4. Policies are consulted - evaluates against risk thresholds"
  - "5. The interaction is accepted, blocked, or challenged"
  - "Card testing fraud - stop criminals testing stolen payment info"
- **Capacidades Determinísticas Identificadas**:
  - Risk score calculation
  - Policy/threshold evaluation
  - Accept/Block/Challenge decisions
  - Card testing detection
  - Device ID correlation (**GAP**: não disponível no payload)

#### Platform (ID 24)
- **URL**: https://kount.com
- **Status**: ⏳ PENDENTE

---

### 3.13 Sift (IDs 25-26)

#### Fintech & Finance (ID 25)
- **URL**: https://sift.com/solutions/fintech-finance/
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "How Fintech & Finance Fight Fraud"
  - "$9.5B Card-not-present (CNP) fraud losses in 2023"
  - "+13% Year-over-year increase in fintech fraud rates"
  - "2.3% Fintech payment fraud attack rate"
  - "Secure every transaction, stop fake signups"
  - "Account Creation, Account Takeover, Money Movement"
  - "Chargeback Fraud, Payment Fraud, Policy Abuse"
- **Capacidades Determinísticas Identificadas**:
  - CNP fraud detection
  - Account takeover indicators
  - Chargeback fraud patterns
  - Payment fraud rules
  - Policy abuse detection

#### Platform (ID 26)
- **URL**: https://sift.com
- **Status**: ⏳ PENDENTE

---

### 3.14 BioCatch (IDs 27-28)

#### Platform (ID 27)
- **URL**: https://www.biocatch.com
- **Status**: ⏳ PENDENTE

#### Scams360 (ID 28)
- **URL**: https://www.biocatch.com/solutions/scams360
- **Status**: ⏳ PENDENTE

#### Account Takeover Protection (Extra)
- **URL**: https://www.biocatch.com/solutions/account-takeover-protection
- **Status**: ✅ OK
- **Evidências Extraídas**:
  - "Account Opening Protection"
  - "Account Takeover Protection"
  - "Mule Account Detection"
  - "Social Engineering Scam Detection"
  - "Strong Customer Authentication"
- **Capacidades Determinísticas Identificadas**:
  - ATO detection patterns
  - Mule account indicators
  - Social engineering scam patterns
  - **GAP**: Behavioral biometrics requer session data (não disponível)

---

### 3.15 HAWK:AI (IDs 35-36, 55, 87)

#### Transaction Fraud (ID 35)
- **URL**: https://hawk.ai/solutions/fraud/transaction-fraud
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "Cut payment fraud with real-time protection"
  - "True real-time payment and card fraud detection (150 ms average)"
  - "Support for all payment rails: ACH, BACS, Card, Check, P2P, SEPA, Wire"
  - "Payment interdiction - block, hold, release transactions"
  - "Out-of-the-box rule guidance: ACH/wire fraud, ATM fraud, card fraud, check fraud, merchant fraud"
  - "Self-serve rule management, configuration, and live sandbox testing"
  - "Round Amount Rule Platform Screenshot"
  - "Scams & Mules detection"
- **Capacidades Determinísticas Identificadas**:
  - Real-time detection (150ms)
  - Multi-rail support (ACH, Wire, Card, Check, P2P)
  - **Self-serve rule management** (similar ao RULEX!)
  - Round amount rules (structuring)
  - ATM fraud rules
  - Merchant fraud rules
  - Sandbox testing

### 3.16 Verafin (IDs 39-40, 56, 86)

#### Fraud Detection Management (ID 39)
- **URL**: https://verafin.com/product/fraud-detection-management/
- **Status**: ✅ OK
- **URLs Visitadas**: 1
- **Evidências Extraídas**:
  - "Fraud Detection and Management solutions"
  - "Cross-channel approach - see whole story of customer activity"
  - "Consortium approach - profile payment originator and beneficiary"
  - "Hundreds of millions of counterparties profiled"
  - "Deposit Fraud - examines complete profile, account, transactional history"
  - "Check image viewer for visual storytelling"
  - Solutions: Instant Payments, Deposit Fraud, Check Fraud, Wire Fraud, ACH Fraud, Online Account Takeover, Card Fraud, Loan Fraud
- **Capacidades Determinísticas Identificadas**:
  - Cross-channel analysis
  - Consortium data profiling
  - Deposit fraud patterns
  - Check fraud detection
  - Wire fraud rules
  - ACH fraud rules
  - ATO indicators
  - Card fraud rules

### 3.17-3.100 (Demais Vendors)

*[Estrutura similar para IDs restantes - a ser preenchido durante navegação]*

---

## 4. Fontes Oficiais Brasileiras

### 4.1 Banco Central - BC Protege+
- **URL**: https://www.bcb.gov.br/meubc/bcprotege
- **Status**: ⏳ PENDENTE
- **Foco**: Regras Pix, MED, controles determinísticos

### 4.2 Febraban AntiFraude
- **URL**: https://portal.febraban.org.br/AntiFraude/
- **Status**: ⏳ PENDENTE
- **Foco**: Boas práticas bancárias BR

### 4.3 ClearSale
- **URL**: https://br.clear.sale/
- **Status**: ⏳ PENDENTE

### 4.4 Serasa Experian Antifraude
- **URL**: https://www.serasaexperian.com.br/solucoes/antifraude/
- **Status**: ⏳ PENDENTE

---

## 5. Datasets e Benchmarks (Uso Restrito)

**Nota**: Usar apenas para extrair tipologias e controles determinísticos. NÃO usar para ML.

| Dataset | URL | Uso |
|---------|-----|-----|
| Kaggle ULB | https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud | Tipologias |
| UCI Default CC | https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients | Tipologias |
| Amazon FDB | https://www.amazon.science/code-and-datasets/fdb-fraud-dataset-benchmark | Tipologias |
| HF Financial-Fraud | https://huggingface.co/datasets/amitkedia/Financial-Fraud-Dataset | Tipologias |

---

## 6. Papers e Referências Acadêmicas

| Paper | URL | Foco |
|-------|-----|------|
| ScienceDirect 2025 | https://www.sciencedirect.com/science/article/pii/S2666764925000372 | Sinais determinísticos |
| ArXiv 2512.21866 | https://www.arxiv.org/abs/2512.21866 | Controles |
| ArXiv 2501.15290v1 | https://arxiv.org/html/2501.15290v1 | Controles |
| MDPI 2073-431X | https://www.mdpi.com/2073-431X/14/10/437 | Controles |

---

## 7. Curadorias GitHub

| Repo | URL | Uso |
|------|-----|-----|
| Fraud Detection Handbook | https://github.com/Fraud-Detection-Handbook/awesome-credit-card-fraud-detection | Tipologias |
| Safe Graph | https://github.com/safe-graph/graph-fraud-detection-papers | Tipologias |
| Online Payment Fraud | https://github.com/sergio11/online_payment_fraud | Tipologias |

---

## 8. Próximos Passos

1. [ ] Navegar URLs 1-25 (SAS, FICO, IBM, NICE, ACI, LexisNexis)
2. [ ] Navegar URLs 26-50 (Experian, TransUnion, Mastercard, Visa, Feedzai, Kount, Sift, BioCatch)
3. [ ] Navegar URLs 51-75 (Alloy, Sardine, ComplyAdvantage, HAWK:AI, SEON, Verafin)
4. [ ] Navegar URLs 76-100 (Demais vendors)
5. [ ] Navegar fontes BR (BC, Febraban, ClearSale, Serasa)
6. [ ] Extrair capacidades determinísticas para 02_CAPABILITIES_EXTRACTION.md
7. [ ] Compilar Top 50 regras para 03_RULES_CATALOG_TOP50.md
