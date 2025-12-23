# Pesquisa Devastadora — Fraude em Cartões (Crédito/Débito/CNP)
**Gerado em:** 23/12/2025  
**Objetivo:** consolidar **URLs** (papers, reports, estudos, benchmarks, datasets, guias oficiais e notícias) para enriquecer um repositório `.md` voltado à **prevenção de fraude em cartões**.

> Observação: links abaixo são majoritariamente **fontes primárias** (PDFs oficiais, arXiv, proceedings, docs de padrão).  
> Para “produção”, use isso como base para criar: **features temporais + contadores + 3DS + device/IP + grafos + chargeback**.

---

## 1) Relatórios oficiais e estudos de mercado (2024–2025)

### Bancos centrais / regulatório
- **ECB — 2025 report on payment fraud (dados 2024 EU/EEA) [PDF]**  
  https://www.ecb.europa.eu/press/intro/publications/pdf/ecb.ebaecb202512.en.pdf

- **European Payments Council — 2024 Payments Threats & Fraud Trends Report [PDF]**  
  https://www.europeanpaymentscouncil.eu/sites/default/files/kb/file/2025-03/EPC162-24%20v1.0%202024%20Payments%20Threats%20and%20Fraud%20Trends%20Report.pdf

### Indústria (cartões / e-commerce)
- **Visa Acceptance — 2025 Global eCommerce Payments & Fraud Report [PDF]**  
  https://www.visaacceptance.com/content/dam/documents/campaign/fraud-report/global-fraud-report-2025.pdf

- **Visa PERC — Biannual Threats Report (Jul–Dec 2024) [PDF]**  
  https://corporate.visa.com/content/dam/VCOM/corporate/solutions/documents/visa-perc-biannual-report-spring-2025.pdf

- **UK Finance — Annual Fraud Report 2025 [PDF]**  
  https://www.ukfinance.org.uk/system/files/2025-05/UK%20Finance%20Annual%20Fraud%20report%202025.pdf  
  Página do relatório (índice + histórico): https://www.ukfinance.org.uk/policy-and-guidance/reports-and-publications/annual-fraud-report-2025

- **UK Finance — Half Year Fraud Report 2025 [PDF]**  
  https://www.ukfinance.org.uk/system/files/2025-10/Half%20Year%20Fraud%20Report%202025_0.pdf

- **AFP — 2025 Payments Fraud and Control Survey (resultados 2024)**  
  https://www.financialprofessionals.org/training-resources/resources/survey-research-economic-data/details/payments-fraud

### Governo / denúncia / estatísticas de fraude (útil para “threat modeling”)
- **FBI IC3 — 2024 Internet Crime Report [PDF]**  
  https://www.ic3.gov/AnnualReport/Reports/2024_IC3Report.pdf  
  Press release: https://www.fbi.gov/news/press-releases/fbi-releases-annual-internet-crime-report

- **FTC — Consumer Sentinel Network Data Book 2024 [PDF]**  
  https://www.ftc.gov/system/files/ftc_gov/pdf/csn-annual-data-book-2024.pdf  
  FTC release (fraud losses 2024): https://www.ftc.gov/news-events/news/press-releases/2025/03/new-ftc-data-show-big-jump-reported-losses-fraud-125-billion-2024

---

## 2) Notícias e casos recentes (para “modus operandi” e inteligência)

- Reuters (ECB): https://www.reuters.com/business/ecb-payment-fraud-rises-42-bln-eur-2024-strong-authentication-remains-effective-2025-12-15/

- Guardian (fraude via cartas bancárias interceptadas + “card not received”):  
  https://www.theguardian.com/money/2025/jan/17/two-men-sentenced-in-uk-over-1m-based-on-stolen-banking-letters

- TechRadar (alerta FBI sobre portais IC3 falsos):  
  https://www.techradar.com/pro/security/scammers-build-fake-fbi-crime-reporting-portals-to-steal-personal-info-warns-fbi

---

## 3) Padrões e guias oficiais (3DS / EMV / PCI) — “camada de sinais fortes”

### 3-D Secure (3DS) / autenticação (CNP)
- **EMVCo — EMV 3-D Secure (visão geral + FAQs)**  
  https://www.emvco.com/emv-technologies/3-d-secure/

- **US Payments Forum — EMV 3DS White Paper (2020) [PDF]**  
  https://www.uspaymentsforum.org/wp-content/uploads/2020/03/EMV-3DS-WP-FINAL-March-2020.pdf

- **Visa — PSD2/SCA Remote Electronic Transactions (CAVV/ECI etc.) [PDF]**  
  https://www.visa.es/content/dam/VCOM/regional/ve/unitedkingdom/PDF/sca/Visa-PSD2-SCA-for-Remote-Electronic-Transactions-Implementation-Guide.pdf

- **Explicação prática de 3DS 2.2 (gateway BR)**  
  https://docs.cielo.com.br/gateway-en/docs/what-is-3ds-22-authentication

### EMV / Contactless (CP)
- **EMVCo — EMV Contactless Chip**  
  https://www.emvco.com/emv-technologies/emv-contactless-chip/

- **SoK (2025) — segurança EMV contactless [PDF]**  
  https://arxiv.org/pdf/2504.12812

### PCI DSS / Compliance (observabilidade, logs, hardening)
- **PCI SSC — SAQ D Service Providers v4.0 [PDF]**  
  https://listings.pcisecuritystandards.org/documents/PCI-DSS-v4-0-SAQ-D-Service-Provider.pdf

- **PCI DSS v4.0.1 (cópia espelhada) [PDF]**  
  https://www.middlebury.edu/sites/default/files/2025-01/PCI-DSS-v4_0_1.pdf

- **AWS — PCI DSS Compliance on AWS v4 (guia) [PDF]**  
  https://d1.awsstatic.com/whitepapers/compliance/pci-dss-compliance-on-aws-v4-102023.pdf

---

## 4) Benchmarks e Feature Extraction em tempo real (RTFE)

- **FEBench (GitHub)**  
  https://github.com/decis-bench/febench

- **FEBench paper (VLDB/PVLDB) [PDF]**  
  https://www.vldb.org/pvldb/vol16/p3597-lu.pdf

- **ACM DOI / resumo**  
  https://dl.acm.org/doi/abs/10.14778/3611540.3611550

---

## 5) Datasets & Benchmarks (treino/validação)

### Benchmark suite
- **Amazon Science — FDB Fraud Dataset Benchmark**  
  https://www.amazon.science/code-and-datasets/fdb-fraud-dataset-benchmark  
  GitHub: https://github.com/amazon-science/fraud-dataset-benchmark

### Datasets populares (cartão)
- **Kaggle — Credit Card Fraud (ULB / 2013)**  
  https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud

- **Kaggle — Credit Card Fraud 2025 (sintético)**  
  https://www.kaggle.com/datasets/prince7489/credit-card-fraud-2025

### Geração sintética (para cenários controlados)
- **Sparkov — Credit Card Transactions generator (GitHub)**  
  https://github.com/namebrandon/Sparkov_Data_Generation

---

## 6) Papers (acadêmicos) — features, drift, 3DS, grafos, transformers

### Feature engineering (clássico, super citável)
- **Bahnsen et al. (2016) — Feature engineering strategies for credit card fraud detection [PDF]**  
  https://albahnsen.github.io/files/Feature%20Engineering%20Strategies%20for%20Credit%20Card%20Fraud%20Detection_published.pdf  
  ScienceDirect: https://www.sciencedirect.com/science/article/pii/S0957417415008386

### Conceito de drift + labels atrasados (cenário real em chargeback/investigação)
- **Pozzolo et al. (2015) — Concept drift + delayed supervised information**  
  PDF (mirror): https://scispace.com/pdf/credit-card-fraud-detection-and-concept-drift-adaptation-1fm3uedi90.pdf  
  SemanticScholar: https://www.semanticscholar.org/paper/Credit-card-fraud-detection-and-concept-drift-with-Pozzolo-Boracchi/b769ec8f3be051b7c5f9a0ba5f85a38594950df9

- **Soemers et al. (2018, AAAI) — contextual bandits + decision trees para drift [PDF]**  
  https://cdn.aaai.org/ojs/11411/11411-13-14939-1-2-20201228.pdf

### 3DS 2.0 (fatores e impacto)
- **Ali (2020) — Investigation of 3-D Secure's Model for Fraud Detection (arXiv)**  
  https://arxiv.org/abs/2009.12390

### Grafos / GNN / Graph Transformer (rings, mule, collusion)
- **CaT-GNN (2024) — causal temporal GNN para card fraud (arXiv)**  
  https://arxiv.org/abs/2402.14708

- **IJCAI 2024 — High-order graph representation learning [PDF]**  
  https://www.ijcai.org/proceedings/2024/0839.pdf

- **ACM (2024) — FraudGT (Graph Transformer)**  
  https://dl.acm.org/doi/10.1145/3677052.3698648

- **Paper 2025 (PDF) — HTGNN real-time fraud detection framework**  
  https://www.scitepress.org/Papers/2025/131613/131613.pdf

### Transformers (sequências/tabular)
- **Yu et al. (2024) — Credit Card Fraud Detection Using Advanced Transformer Model (arXiv)**  
  https://arxiv.org/abs/2406.03733  
  PDF (espelho): https://scispace.com/pdf/credit-card-fraud-detection-using-advanced-transformer-model-2ueae0zzyh.pdf

---

## 7) Chargebacks / disputas (labels, reason codes, supervisão atrasada)

- **Chase/Paymentech — Chargeback Reason Code User Guide [PDF]**  
  https://merchantservices.chase.com/content/dam/chase/merchant-services/support/us/documents/chargeback-reason-code-user-guide.pdf

- Guias gerais (não oficiais, mas úteis para referência rápida):
  - https://www.chargebackgurus.com/chargeback-reason-codes
  - https://chargebacks911.com/chargeback-reason-codes/

---

## 8) “Como transformar isso em features” (checklist curto)

### Sinais 3DS (CNP)
- ECI (5/6 vs 7), CAVV/TAVV, frictionless vs challenge, versão 3DS, DS score, ACS status.

### Temporal/contadores (o núcleo)
- tx_count_{5m,1h,24h,7d} por customer/card/device/ip/merchant/mcc  
- sum_amount_{1h,24h,7d}, avg_amount, std_amount, zscore vs histórico  
- unique_merchants_{24h,7d}, unique_countries_{7d,30d}  
- time_since_last_tx, decline_streak, decline_rate

### Device/IP
- device_unique_customers_7d, ip_unique_cards_24h, vpn/proxy/tor_flag, geovelocidade/“impossible travel”.

### Grafos
- grau do nó, componentes conectados, PageRank/Betweenness, triângulos, comunidades, “shared device”, “shared ip”.

### Chargeback / label delay
- Separar “feedback humano rápido” vs “label final (chargeback)” (vide papers de drift).

---

**Fim.**
