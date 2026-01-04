# Fraud Research URL Collector

Sistema automatizado para coletar 1000+ URLs únicos sobre **fraudes bancárias**, **detecção de fraude**, **datasets**, **papers científicos** e **conteúdo técnico**.

## 📋 Visão Geral

Este projeto fornece múltiplas abordagens para coletar URLs sobre fraude:

1. **Fontes Curadas** - JSONs com URLs verificados e categorizados
2. **Coleta via APIs** - Scripts que consultam arXiv, Semantic Scholar, CrossRef, GitHub
3. **Geração Final** - Merge de todas as fontes com deduplicação

## 🗂️ Estrutura

```
fraud-research-collector/
├── README.md                    # Este arquivo
├── requirements.txt             # Dependências Python
├── curated_sources.json         # Fontes curadas principais (~400 URLs)
├── additional_sources.json      # Fontes adicionais (~350 URLs)
├── fraud_url_collector.py       # Coletor com APIs (assíncrono)
├── extended_collector.py        # Coletor estendido
└── merge_and_generate.py        # Merge e geração do CSV final
```

## 🚀 Uso Rápido

### Opção 1: Gerar CSV a partir das fontes curadas (mais rápido)

```bash
cd fraud-research-collector
python merge_and_generate.py --output fraud_urls_1000.csv
```

### Opção 2: Coletar via APIs (mais lento, mais URLs)

```bash
pip install -r requirements.txt
python extended_collector.py --output fraud_urls.csv --target 1000
```

### Opção 3: Coletor completo com APIs

```bash
pip install -r requirements.txt
python fraud_url_collector.py --output fraud_urls.csv --target 1000
```

## 📊 Categorias

O CSV final contém URLs distribuídos em 6 categorias:

| Categoria     | Meta  | Descrição |
|---------------|-------|-----------|
| PAPER         | 300   | Papers científicos (arXiv, IEEE, ACM, journals) |
| DATASET       | 200   | Datasets e benchmarks (Kaggle, UCI, GitHub) |
| WHITEPAPER    | 150   | Relatórios oficiais, reguladores, consultorias |
| TECH          | 150   | Blogs de engenharia, tutoriais, guias técnicos |
| CASES         | 100   | Casos reais, alertas, threat intelligence |
| FUNDAMENTALS  | 100   | Conceitos, fundamentos, documentações |

## 📄 Formato de Saída

O CSV segue o formato exato especificado:

```csv
categoria,url
PAPER,https://arxiv.org/abs/xxxx.xxxxx
DATASET,https://www.kaggle.com/datasets/...
WHITEPAPER,https://...pdf
TECH,https://...
CASES,https://...
FUNDAMENTALS,https://...
```

Também é gerado um arquivo `.detailed.csv` com metadados adicionais:

```csv
categoria,url,title,source
PAPER,https://arxiv.org/abs/2312.01234,Credit Card Fraud Detection,arXiv
...
```

## 🔍 Fontes de Dados

### Papers Científicos
- **arXiv** - Preprints de ML, AI, Segurança
- **Semantic Scholar** - Busca semântica de papers
- **CrossRef** - Journals e conferências (DOI)
- **IEEE/ACM/Springer/Elsevier** - Publicações peer-reviewed
- **MDPI/Nature/PLOS** - Open access journals

### Datasets
- **Kaggle** - Competições e datasets públicos
- **UCI ML Repository** - Datasets clássicos
- **GitHub** - Repositórios com dados
- **OpenML** - Benchmark datasets
- **HuggingFace** - Datasets para ML

### Whitepapers e Relatórios
- **BIS** - Bank for International Settlements
- **FATF** - Financial Action Task Force
- **FinCEN** - Financial Crimes Enforcement Network
- **ECB** - European Central Bank
- **Consultorias** - McKinsey, PwC, Deloitte, EY, KPMG
- **Vendors** - FICO, Feedzai, SAS, Nice Actimize

### Conteúdo Técnico
- **Engineering Blogs** - Spotify, Netflix, Uber, Grab, Stripe
- **Cloud Providers** - AWS, GCP, Azure
- **ML Platforms** - Databricks, H2O, DataRobot
- **Tutorials** - TensorFlow, PyTorch, Scikit-learn

## 🛠️ APIs Utilizadas

| API | Rate Limit | Uso |
|-----|------------|-----|
| arXiv | 3s entre requests | Papers de ML/AI/Security |
| Semantic Scholar | 100 req/5min | Papers com citações |
| CrossRef | Polite pool | DOIs e metadados |
| GitHub | 60 req/hora (sem auth) | Repositórios |

## ⚙️ Configuração

### Dependências

```bash
pip install -r requirements.txt
```

Pacotes principais:
- `aiohttp` - HTTP assíncrono
- `requests` - HTTP síncrono
- `tqdm` - Barras de progresso
- `tenacity` - Retry com backoff

### Variáveis de Ambiente (opcional)

```bash
export GITHUB_TOKEN="ghp_..."  # Para maior rate limit
export SEMANTIC_SCHOLAR_API_KEY="..."  # Se disponível
```

## 📈 Estatísticas de Coleta

Execução típica:

```
======================================================================
FRAUD URL COLLECTION SUMMARY
======================================================================

📊 Total URLs collected: 1000
🎯 Target: 1000
✅ Status: MET

📁 By Category:
  ✓ PAPER           300/ 300 [████████████████████] 100.0%
  ✓ DATASET         200/ 200 [████████████████████] 100.0%
  ✓ WHITEPAPER      150/ 150 [████████████████████] 100.0%
  ✓ TECH            150/ 150 [████████████████████] 100.0%
  ✓ CASES           100/ 100 [████████████████████] 100.0%
  ✓ FUNDAMENTALS    100/ 100 [████████████████████] 100.0%

🌐 By Source:
  curated_sources             350
  additional_sources          300
  arXiv                       150
  GitHub                      100
  SemanticScholar              50
  CrossRef                     50
======================================================================
```

## 🔒 Anti-Alucinação

Este sistema **não inventa URLs**. Todas as URLs vêm de:

1. ✅ Fontes curadas e verificadas manualmente
2. ✅ APIs oficiais (arXiv, Semantic Scholar, CrossRef, GitHub)
3. ✅ Padrões conhecidos de URLs válidos

## 📝 Licença

MIT License - Use livremente para pesquisa e desenvolvimento.

## 🤝 Contribuição

Para adicionar mais fontes, edite os arquivos JSON:

```json
{
  "sources": {
    "PAPER": [
      {"url": "https://...", "title": "Paper Title"}
    ]
  }
}
```

## 📧 Suporte

Para dúvidas ou sugestões, abra uma issue no repositório.
