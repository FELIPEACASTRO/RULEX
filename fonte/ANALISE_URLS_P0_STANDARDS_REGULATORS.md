# ANALISE_URLS_P0_STANDARDS_REGULATORS (evidence-first)

Objetivo: extrair **apenas o que está verificável** nas páginas acessadas (sem inferências “soltas”), e traduzir em **candidatos de regras duras** e **requisitos de dados** para o RULEX.

Escopo desta passada: 5 URLs P0 (standards/regulators) acessadas via captura de conteúdo.

---

## 1) EMVCo — EMV® 3-D Secure (visão geral)

Fonte: https://www.emvco.com/emv-technologies/3-d-secure/

### 1.1 Evidências (o que a página afirma)

- EMV 3DS é usado por emissores e merchants para **autenticar consumidores** e “safeguard against” **fraude CNP (card-not-present)**.
- EMV 3DS “enables the exchange of data (messages) between the merchant and the issuer” para autenticar consumidor e aprovar transação.
- A página explicita que os dados incluem **informação sobre a transação, método de pagamento e device**.
- Há referências a **flows** de autenticação e a recursos de “UI/UX Guidelines”, “Secure Browser Best Practices” e whitepapers.
- Há um conceito de **aprovação/avaliação de produtos 3DS** (processos de compliance/testes) e listagens “Approved/Evaluated Products”, “Service Providers”, “Registered IDs”.

### 1.2 Candidatos de regras duras (derivados com cautela)

- Regra candidata: **Se canal = e-commerce/CNP e “3DS data” estiver ausente**, aumentar rigor (step-up / bloqueio / revisão manual conforme política).
  - Observação: a página afirma troca de dados e uso para autenticação; **não define** quais campos (ex.: ECI/CAVV/DS Trans ID) — ver GAP.

- Regra candidata: **Se o “device context” estiver ausente/degenerado** (sem dados mínimos do dispositivo), tratar como risco maior.
  - Base: a página diz que o dado inclui informação do device.

- Regra candidata: **Atribuir política por segmento** (CNP vs demais) para exigir/otimizar autenticação, já que o objetivo citado é reduzir CNP fraud com menor fricção.

### 1.3 Dados que o RULEX precisa (mínimo sugerido) + GAPs

- Necessário (alto nível, pela evidência):
  - Canal (CNP/e-commerce), dados da transação, método de pagamento, device.
- NÃO ENCONTRADO nesta página (necessário para regras mais “cirúrgicas”):
  - Campos 3DS específicos (ECI, CAVV/AAV, DS Trans ID, 3DS version, transStatus, etc.).
  - Valores/limiares recomendados (thresholds).

---

## 2) EMVCo — “Optimising Online Payment Authentication with EMV® 3-D Secure”

Fonte: https://www.emvco.com/knowledge-hub/optimising-online-payment-authentication-with-emv-3-d-secure/

### 2.1 Evidências (o que a página afirma)

- Existem dois fluxos primários no 3DS: **Frictionless Flow** e **Challenge Flow**.
- **Frictionless Flow**: emissores aceitam transações **sem desafiar** o portador; isso é alcançado por **real-time risk assessment**.
- **Challenge Flow**: é disparado quando a transação é “deemed high-risk” pelo emissor ou precisa de confirmação; requer **informação adicional** do portador ao emissor.
  - Exemplos citados: **OTP via mobile** e validação via **mobile banking app** (OOB authentication).
- A página afirma que o Challenge Flow “increases security” e ajuda a reduzir fraude.
- A atualização citada menciona suporte a métodos como:
  - **WebAuthn / Secure Payment Confirmation (SPC)** (SPC como padrão W3C “built on WebAuthn”), para “better determine the legitimacy of a transaction”.
  - **Decoupled Authentication**: autenticação separada do fluxo de pagamento; alternativa quando método primário falha/não é possível; habilita autenticação iniciada pelo merchant quando cardholder não está presente (ex.: alguns cenários MOTO e assinaturas).
- “3DS message extensions”: mecanismo para estender troca de dados sem exigir nova versão; a página cita que EMV 3DS possui “over a hundred data elements”, mas que nem tudo é coberto; por isso extensões.
- “Split-SDK”: arquitetura alternativa dividindo funções entre client-side e server-side; facilita updates server-side e suporta diversos canais/dispositivos.

### 2.2 Candidatos de regras duras (derivados com cautela)

- Regra candidata: **Usar o “flow outcome” como sinal determinístico**:
  - Se transação passa por **Challenge Flow**, tratar como risco inerentemente maior que “Frictionless”, pois a própria página associa Challenge a high-risk/need confirmation.

- Regra candidata: **Exigir step-up** (ou negar) quando:
  - transação for marcada como “high-risk” (conforme seu risco interno) e não houver capacidade de Challenge/OOB.
  - Observação: a evidência mostra que Challenge existe para high-risk; a decisão final é de política do RULEX.

- Regra candidata: **Permitir política específica para recorrência/assinaturas**:
  - Considerar autenticação “decoupled”/merchant-initiated onde aplicável.
  - NÃO ENCONTRADO: quais flags/campos implementacionais no payload.

- Regra candidata: **Aumentar rigor quando dados de autenticação forte (ex.: WebAuthn/SPC) não estão presentes em cenários que você quer “hardening”**.
  - Observação: a página diz que WebAuthn/SPC pode ser usado para reduzir risco de fraude; não define obrigações.

- Regra candidata: **“Message extensions present”** como pista de uso-case/regulatório, caso a integração forneça isso.

### 2.3 Dados que o RULEX precisa (mínimo sugerido) + GAPs

- Necessário (pela evidência):
  - Um campo que represente o **tipo de fluxo** (frictionless vs challenge), e algum indicador de método de challenge (OTP/OOB/etc.).
- NÃO ENCONTRADO nesta página:
  - Mapeamento campo-a-campo para payloads 3DS reais.
  - Quais “message extensions” são enviadas e como identificá-las no tráfego.
  - Limiares concretos (ex.: “quando desafiar”).

---

## 3) UK Finance — Annual Fraud Report 2025 (página do relatório)

Fonte: https://www.ukfinance.org.uk/policy-and-guidance/reports-and-publications/annual-fraud-report-2025

### 3.1 Evidências (o que a página afirma)

- A página afirma que “Fraud continues to pose a major threat” com **“over £1 billion stolen in 2024”**, e no corpo cita **“over £1.1 billion of losses in 2024”**.
- Afirma que **APP fraud** (manipulação da vítima para enviar dinheiro) **caiu** em valor total e número de casos.
- Afirma que outros tipos, “notably **remote purchase fraud**”, **aumentaram**.
- Afirma que criminosos **mudam táticas**; “closing one vulnerability in isolation only leads them adapting and exploiting others”.
- Linka para o PDF do relatório.

### 3.2 Candidatos de regras duras (derivados com cautela)

- Regra candidata: **Tratar “remote purchase” (compra remota) como categoria em alta**, portanto elevar baseline de rigor nesse canal.
  - Ex.: thresholds mais baixos para bloqueio/step-up em e-commerce vs presença.

- Regra candidata (operacional): **Regras precisam ser revisadas continuamente** (táticas mudam quando você fecha um vetor).
  - Isso vira uma regra de governança do RULEX: versão + revisão periódica obrigatória.

### 3.3 Dados que o RULEX precisa + GAPs

- Necessário (alto nível): campo de **canal** / categoria (ex.: remote purchase).
- NÃO ENCONTRADO nesta página:
  - Breakdown numérico por subcategoria (valores, proporções, vetores específicos).
  - Sinais práticos (ex.: device/IP) — provavelmente no PDF, não no resumo da página.

---

## 4) FTC — Press release sobre perdas reportadas de fraude (2024)

Fonte: https://www.ftc.gov/news-events/news/press-releases/2025/03/new-ftc-data-show-big-jump-reported-losses-fraud-125-billion-2024

### 4.1 Evidências (o que a página afirma)

- Consumidores reportaram **> $12.5B** em perdas para fraude em 2024 (25% vs ano anterior).
- O número **não** foi por aumento de reports; os reports ficaram “stable”; o que subiu foi a % de pessoas que reportam perder dinheiro:
  - 2023: **27%** dos que reportaram fraude perderam dinheiro.
  - 2024: **38%**.
- Categoria com maior perda: **investment scams** (cita **$5.7B**).
- Segunda maior: **imposter scams** (**$2.95B**).
- Afirma que perdas em scams pagos com **bank transfers ou cryptocurrency** superaram todos os outros métodos combinados.
- “Online shopping issues” é citada como uma das categorias mais comuns.

### 4.2 Candidatos de regras duras (derivados com cautela)

- Para RULEX (se opera em pagamentos):
  - Regra candidata: **Classificar e elevar risco para “online shopping issues”** como etiqueta de cluster/família de fraude.
  - Regra candidata (quando aplicável): **Método de pagamento** como sinal forte (transferência/cripto) — se RULEX só cobre cartão, isto vira apenas contexto.

### 4.3 Dados que o RULEX precisa + GAPs

- Necessário: taxonomia interna para mapear incidentes/casos em categorias (imposter, online shopping issues, etc.).
- NÃO ENCONTRADO nesta página:
  - Atributos transacionais específicos de cartão (3DS, MCC, etc.).

---

## 5) FBI — Press release: Annual Internet Crime Report (IC3)

Fonte: https://www.fbi.gov/news/press-releases/fbi-releases-annual-internet-crime-report

### 5.1 Evidências (o que a página afirma)

- O release referencia o **2024 Internet Crime Report** (PDF no IC3).
- Afirma: **859,532 complaints** e perdas reportadas **> $16B** (33% a mais que 2023).
- Top 3 cyber crimes por volume: **phishing/spoofing**, **extortion**, **personal data breaches**.
- Maior perda: **investment fraud** (especialmente com crypto), **> $6.5B**.
- Grupo etário: pessoas **> 60 anos** com maiores perdas (~ **$5B**) e maior número de complaints.

### 5.2 Candidatos de regras duras (derivados com cautela)

- Regra candidata: **Sinalizar maior risco quando há evidência de phishing/spoofing** (ex.: login recente suspeito, mudança de credenciais, email/telefone recém-trocado) — desde que o RULEX tenha esses eventos.
- Regra candidata: **Se houver dado de faixa etária**, aumentar rigor para >60 em fluxos de alto risco (ex.: alteração de dados, transações incomuns).
  - Observação: isso é sensível e depende do seu modelo de negócio/privacidade; a evidência aqui é de impacto agregado, não um “mandato”.

### 5.3 Dados que o RULEX precisa + GAPs

- Necessário (se adotar essas regras): eventos de conta (alteração de contato, recuperação de senha), sinais de takeover, eventualmente idade.
- NÃO ENCONTRADO nesta página:
  - Controles de autenticação por tipo de pagamento (isso deve estar no PDF do IC3 ou fora do escopo do release).

---

## 6) Consolidação: “Sinais” e “Regras” que já são sustentáveis por evidência

### 6.1 Sinais sustentáveis com as páginas lidas

- Canal CNP / e-commerce (EMVCo).
- Presença/ausência de dados de autenticação (alto nível) e device context (EMVCo).
- Resultado/fluxo 3DS: frictionless vs challenge (EMVCo knowledge hub).
- Categoria “remote purchase fraud” em alta (UK Finance página).
- Taxonomias amplas de fraude (FTC/FBI) para priorizar casos e rotas (contexto, não card-específico).

### 6.2 Regras duras “core” (sem thresholds numéricos)

- R-3DS-001: Se canal = CNP e autenticação/3DS ausente → step-up/bloqueio/revisão.
- R-3DS-002: Se flow = challenge → elevar risco e aplicar controles adicionais.
- R-CTX-001: Se device context ausente/insuficiente → elevar risco.
- R-CHG-001: Políticas específicas para recorrência/merchant-initiated auth (decoupled), quando aplicável.

---

## 7) Próximos deep dives (necessários para tornar isso “implementável”)

- Abrir e extrair evidências do PDF do UK Finance Annual Fraud Report 2025.
- Abrir e extrair evidências do PDF do IC3 2024 report.
- Abrir e extrair evidências do EMV 3DS White Paper e (se necessário) especificações/FAQs, para mapear:
  - campos 3DS “de verdade” (ex.: version, transStatus, identifiers, etc.)
  - mapeamento frictionless/challenge no payload
  - eventos de falha/timeout/decoupled

Registro de GAP (desta passada):
- Campos 3DS concretos e mapeamento técnico: NÃO ENCONTRADO nas páginas lidas.
- Thresholds recomendados: NÃO ENCONTRADO nas páginas lidas.
