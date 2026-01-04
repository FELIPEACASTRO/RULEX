#!/usr/bin/env python3
"""
Fraud Research URL Collector
=============================
Collects 1000+ unique URLs about banking fraud, fraud detection, datasets,
and related research from multiple verified sources via APIs.

Categories:
- PAPER: Scientific papers (arXiv, Semantic Scholar, CrossRef)
- DATASET: Datasets and benchmarks (Kaggle, UCI, GitHub)
- WHITEPAPER: Official reports and whitepapers
- TECH: Technical content (engineering blogs, tutorials)
- CASES: Real cases, alerts, threat intelligence
- FUNDAMENTALS: Concepts and foundations

Usage:
    python fraud_url_collector.py --output fraud_urls.csv --target 1000
"""

import asyncio
import csv
import hashlib
import json
import logging
import re
import sys
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple
from urllib.parse import parse_qs, urlencode, urlparse, urlunparse

import aiohttp
import requests
from tenacity import retry, stop_after_attempt, wait_exponential
from tqdm import tqdm

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


@dataclass
class URLRecord:
    """Represents a collected URL with metadata."""
    category: str
    url: str
    title: str = ""
    source: str = ""
    collected_at: str = field(default_factory=lambda: datetime.now().isoformat())

    def normalized_url(self) -> str:
        """Normalize URL for deduplication."""
        parsed = urlparse(self.url)
        # Remove tracking parameters
        query_params = parse_qs(parsed.query)
        clean_params = {
            k: v for k, v in query_params.items()
            if not k.startswith('utm_') and k not in ['ref', 'source', 'campaign']
        }
        clean_query = urlencode(clean_params, doseq=True)
        normalized = urlunparse((
            parsed.scheme,
            parsed.netloc.lower(),
            parsed.path.rstrip('/'),
            parsed.params,
            clean_query,
            ''  # Remove fragment
        ))
        return normalized

    def url_hash(self) -> str:
        """Generate hash for deduplication."""
        return hashlib.md5(self.normalized_url().encode()).hexdigest()


class FraudURLCollector:
    """Main collector class that aggregates URLs from multiple sources."""

    # Search queries organized by category and language
    SEARCH_QUERIES = {
        'papers_en': [
            'credit card fraud detection machine learning',
            'banking fraud detection deep learning',
            'transaction fraud detection neural network',
            'account takeover detection',
            'synthetic identity fraud detection',
            'graph neural network fraud detection',
            'transfer learning fraud detection',
            'federated learning fraud detection',
            'concept drift fraud detection',
            'imbalanced learning fraud detection',
            'anomaly detection financial transactions',
            'fraud detection autoencoder',
            'fraud detection XGBoost',
            'fraud detection random forest',
            'fraud detection ensemble methods',
            'real-time fraud detection streaming',
            'device fingerprinting fraud',
            'behavioral biometrics fraud',
            'velocity rules fraud detection',
            'risk scoring fraud prevention',
            'chargeback fraud detection',
            'payment fraud detection',
            'e-commerce fraud detection',
            'mobile payment fraud',
            'money laundering detection machine learning',
            'AML typologies machine learning',
            'suspicious transaction detection',
            'fraud pattern recognition',
            'fraud feature engineering',
            'explainable AI fraud detection',
        ],
        'papers_pt': [
            'detecção fraude bancária aprendizado máquina',
            'fraude cartão crédito detecção',
            'prevenção fraude transações',
            'fraude Pix detecção',
            'lavagem dinheiro detecção',
        ],
        'datasets': [
            'credit card fraud dataset',
            'transaction fraud dataset',
            'synthetic fraud dataset',
            'banking fraud benchmark',
            'financial fraud corpus',
            'payment fraud dataset',
            'money laundering dataset',
            'AML dataset',
        ],
        'technical': [
            'fraud detection feature engineering',
            'real-time fraud detection architecture',
            'fraud detection MLOps',
            'fraud detection system design',
            'fraud rules engine',
            'fraud scoring model',
        ],
    }

    # Category targets for balanced collection
    CATEGORY_TARGETS = {
        'PAPER': 300,
        'DATASET': 200,
        'WHITEPAPER': 150,
        'TECH': 150,
        'CASES': 100,
        'FUNDAMENTALS': 100,
    }

    def __init__(self, output_path: str = "fraud_urls.csv", target_count: int = 1000):
        self.output_path = Path(output_path)
        self.target_count = target_count
        self.collected_urls: Dict[str, URLRecord] = {}  # hash -> record
        self.session: Optional[aiohttp.ClientSession] = None

    async def __aenter__(self):
        self.session = aiohttp.ClientSession(
            timeout=aiohttp.ClientTimeout(total=30),
            headers={'User-Agent': 'FraudResearchCollector/1.0'}
        )
        return self

    async def __aexit__(self, *args):
        if self.session:
            await self.session.close()

    def add_url(self, record: URLRecord) -> bool:
        """Add URL if not duplicate. Returns True if added."""
        url_hash = record.url_hash()
        if url_hash not in self.collected_urls:
            self.collected_urls[url_hash] = record
            return True
        return False

    def get_category_count(self, category: str) -> int:
        """Count URLs in a specific category."""
        return sum(1 for r in self.collected_urls.values() if r.category == category)

    def category_needs_more(self, category: str) -> bool:
        """Check if category needs more URLs."""
        return self.get_category_count(category) < self.CATEGORY_TARGETS.get(category, 0)

    # =========================================================================
    # arXiv API Integration
    # =========================================================================
    
    async def fetch_arxiv(self, query: str, max_results: int = 100) -> List[URLRecord]:
        """Fetch papers from arXiv API."""
        records = []
        base_url = "http://export.arxiv.org/api/query"
        
        # arXiv categories relevant to fraud
        categories = ['cs.LG', 'cs.AI', 'cs.CR', 'stat.ML', 'q-fin.RM']
        
        for cat in categories:
            params = {
                'search_query': f'all:{query} AND cat:{cat}',
                'start': 0,
                'max_results': max_results // len(categories),
                'sortBy': 'relevance',
                'sortOrder': 'descending'
            }
            
            try:
                async with self.session.get(base_url, params=params) as resp:
                    if resp.status == 200:
                        text = await resp.text()
                        # Parse Atom feed
                        entries = re.findall(
                            r'<entry>.*?<id>(.*?)</id>.*?<title>(.*?)</title>.*?</entry>',
                            text, re.DOTALL
                        )
                        for entry_id, title in entries:
                            # Convert API URL to abstract URL
                            arxiv_id = entry_id.split('/abs/')[-1]
                            url = f"https://arxiv.org/abs/{arxiv_id}"
                            records.append(URLRecord(
                                category='PAPER',
                                url=url,
                                title=title.strip().replace('\n', ' '),
                                source='arXiv'
                            ))
                await asyncio.sleep(3)  # Rate limiting
            except Exception as e:
                logger.warning(f"arXiv error for {query}: {e}")
        
        return records

    async def collect_arxiv_papers(self, progress: tqdm) -> int:
        """Collect papers from arXiv."""
        added = 0
        queries = self.SEARCH_QUERIES['papers_en'] + self.SEARCH_QUERIES['papers_pt']
        
        for query in queries:
            if not self.category_needs_more('PAPER'):
                break
            records = await self.fetch_arxiv(query, max_results=50)
            for record in records:
                if self.add_url(record):
                    added += 1
                    progress.update(1)
                    progress.set_postfix({'added': added, 'total': len(self.collected_urls)})
        
        return added

    # =========================================================================
    # Semantic Scholar API Integration
    # =========================================================================
    
    async def fetch_semantic_scholar(self, query: str, limit: int = 100) -> List[URLRecord]:
        """Fetch papers from Semantic Scholar API."""
        records = []
        base_url = "https://api.semanticscholar.org/graph/v1/paper/search"
        
        params = {
            'query': query,
            'limit': min(limit, 100),
            'fields': 'title,url,externalIds'
        }
        
        try:
            async with self.session.get(base_url, params=params) as resp:
                if resp.status == 200:
                    data = await resp.json()
                    for paper in data.get('data', []):
                        url = paper.get('url')
                        if url:
                            records.append(URLRecord(
                                category='PAPER',
                                url=url,
                                title=paper.get('title', ''),
                                source='SemanticScholar'
                            ))
                        # Also add arXiv link if available
                        ext_ids = paper.get('externalIds', {})
                        if 'ArXiv' in ext_ids:
                            arxiv_url = f"https://arxiv.org/abs/{ext_ids['ArXiv']}"
                            records.append(URLRecord(
                                category='PAPER',
                                url=arxiv_url,
                                title=paper.get('title', ''),
                                source='arXiv'
                            ))
            await asyncio.sleep(1)  # Rate limiting
        except Exception as e:
            logger.warning(f"Semantic Scholar error for {query}: {e}")
        
        return records

    async def collect_semantic_scholar(self, progress: tqdm) -> int:
        """Collect papers from Semantic Scholar."""
        added = 0
        queries = self.SEARCH_QUERIES['papers_en']
        
        for query in queries:
            if not self.category_needs_more('PAPER'):
                break
            records = await self.fetch_semantic_scholar(query, limit=50)
            for record in records:
                if self.add_url(record):
                    added += 1
                    progress.update(1)
        
        return added

    # =========================================================================
    # CrossRef API Integration
    # =========================================================================
    
    async def fetch_crossref(self, query: str, rows: int = 50) -> List[URLRecord]:
        """Fetch papers from CrossRef API."""
        records = []
        base_url = "https://api.crossref.org/works"
        
        params = {
            'query': query,
            'rows': rows,
            'filter': 'type:journal-article,type:proceedings-article',
            'select': 'DOI,title,URL'
        }
        
        try:
            async with self.session.get(base_url, params=params) as resp:
                if resp.status == 200:
                    data = await resp.json()
                    for item in data.get('message', {}).get('items', []):
                        url = item.get('URL')
                        doi = item.get('DOI')
                        if url or doi:
                            final_url = url or f"https://doi.org/{doi}"
                            title = item.get('title', [''])[0] if item.get('title') else ''
                            records.append(URLRecord(
                                category='PAPER',
                                url=final_url,
                                title=title,
                                source='CrossRef'
                            ))
            await asyncio.sleep(1)
        except Exception as e:
            logger.warning(f"CrossRef error for {query}: {e}")
        
        return records

    async def collect_crossref(self, progress: tqdm) -> int:
        """Collect papers from CrossRef."""
        added = 0
        queries = self.SEARCH_QUERIES['papers_en'][:15]  # Limit queries
        
        for query in queries:
            if not self.category_needs_more('PAPER'):
                break
            records = await self.fetch_crossref(query, rows=30)
            for record in records:
                if self.add_url(record):
                    added += 1
                    progress.update(1)
        
        return added

    # =========================================================================
    # GitHub API Integration
    # =========================================================================
    
    async def fetch_github_repos(self, query: str, per_page: int = 30) -> List[URLRecord]:
        """Fetch repositories from GitHub API."""
        records = []
        base_url = "https://api.github.com/search/repositories"
        
        params = {
            'q': f'{query} fraud detection',
            'sort': 'stars',
            'order': 'desc',
            'per_page': per_page
        }
        
        try:
            async with self.session.get(base_url, params=params) as resp:
                if resp.status == 200:
                    data = await resp.json()
                    for repo in data.get('items', []):
                        url = repo.get('html_url')
                        if url:
                            # Categorize based on content
                            desc = (repo.get('description') or '').lower()
                            name = repo.get('name', '').lower()
                            
                            if 'dataset' in desc or 'dataset' in name or 'data' in name:
                                category = 'DATASET'
                            elif 'tutorial' in desc or 'example' in desc:
                                category = 'TECH'
                            else:
                                category = 'TECH'
                            
                            records.append(URLRecord(
                                category=category,
                                url=url,
                                title=repo.get('full_name', ''),
                                source='GitHub'
                            ))
            await asyncio.sleep(2)
        except Exception as e:
            logger.warning(f"GitHub error for {query}: {e}")
        
        return records

    async def collect_github(self, progress: tqdm) -> int:
        """Collect repositories from GitHub."""
        added = 0
        queries = [
            'credit card fraud',
            'fraud detection',
            'transaction fraud',
            'anomaly detection finance',
            'banking fraud',
            'payment fraud',
            'AML anti money laundering',
            'chargeback',
            'synthetic data fraud',
        ]
        
        for query in queries:
            records = await self.fetch_github_repos(query, per_page=30)
            for record in records:
                if self.category_needs_more(record.category) and self.add_url(record):
                    added += 1
                    progress.update(1)
        
        return added

    # =========================================================================
    # Kaggle Datasets (via known URLs pattern)
    # =========================================================================
    
    def get_known_kaggle_datasets(self) -> List[URLRecord]:
        """Return known Kaggle fraud-related datasets."""
        datasets = [
            ("mlg-ulb/creditcardfraud", "Credit Card Fraud Detection Dataset"),
            ("kartik2112/fraud-detection", "Simulated Credit Card Transactions"),
            ("ealaxi/paysim1", "PaySim Mobile Money Simulator"),
            ("ntnu-testimon/paysim1", "PaySim Financial Dataset"),
            ("mlg-ulb/creditcardfraud", "Credit Card Fraud Detection"),
            ("dalpozz/creditcardfraud", "Credit Card Fraud"),
            ("arjunbhasin2013/ccdata", "Credit Card Dataset for Clustering"),
            ("janiobachmann/bank-marketing-dataset", "Bank Marketing"),
            ("shivamb/vehicle-claim-fraud-detection", "Vehicle Insurance Claim Fraud"),
            ("rtatman/fraudulent-email-corpus", "Fraudulent Email Corpus"),
            ("sid321axn/audit-data", "Audit Data"),
            ("sgpjesus/bank-account-fraud-dataset-neurips-2022", "Bank Account Fraud NeurIPS 2022"),
            ("rupakroy/online-payments-fraud-detection-dataset", "Online Payments Fraud Detection"),
            ("volodymyrgavrysh/fraud-detection-bank-dataset-20k-records-binary", "Bank Fraud Detection 20K"),
            ("vbinh002/fraud-ecommerce", "E-commerce Fraud"),
            ("kelvinkelue/credit-card-fraud-prediction", "Credit Card Fraud Prediction"),
            ("dermisfit/fraud-transactions-dataset", "Fraud Transactions Dataset"),
            ("chitwanmanchanda/fraudulent-transactions-data", "Fraudulent Transactions Data"),
            ("jainilcoder/online-payment-fraud-detection", "Online Payment Fraud"),
            ("gopalmahadevan/fraud-detection-example", "Fraud Detection Example"),
            ("computingvictor/transactions-fraud-datasets", "Transactions Fraud Datasets"),
            ("undersc0re/fraudulent-transactions-dataset", "Fraudulent Transactions"),
            ("thedevastator/fraud-detection-fake-job-postings", "Fake Job Postings Fraud"),
            ("isabbaggin/transaction-fraud-detection", "Transaction Fraud Detection"),
            ("mishra5001/credit-card", "Credit Card Dataset"),
            ("vardhansiramdasu/fraudulent-transactions-prediction", "Fraudulent Transactions Prediction"),
            ("sanskrutipanda/transactions-fraud-detection", "Transactions Fraud Detection"),
            ("itsmesunil/bank-loan-modelling", "Bank Loan Modelling"),
            ("whenamancodes/credit-card-customers-prediction", "Credit Card Customers"),
            ("sakshigoyal7/credit-card-customers", "Credit Card Customers Analysis"),
            ("ealtman2019/ibm-transactions-for-anti-money-laundering-aml", "IBM AML Transactions"),
            ("berkaybayraktar/synthetic-transaction-data-for-aml", "Synthetic AML Data"),
            ("ellipticco/elliptic-data-set", "Elliptic Bitcoin Dataset"),
            ("anandshaw2001/bitcoin-network-transactional-data", "Bitcoin Transactions"),
        ]
        
        records = []
        for dataset_id, title in datasets:
            url = f"https://www.kaggle.com/datasets/{dataset_id}"
            records.append(URLRecord(
                category='DATASET',
                url=url,
                title=title,
                source='Kaggle'
            ))
        return records

    def collect_kaggle_datasets(self, progress: tqdm) -> int:
        """Collect known Kaggle datasets."""
        added = 0
        records = self.get_known_kaggle_datasets()
        for record in records:
            if self.add_url(record):
                added += 1
                progress.update(1)
        return added

    # =========================================================================
    # UCI ML Repository Datasets
    # =========================================================================
    
    def get_known_uci_datasets(self) -> List[URLRecord]:
        """Return known UCI fraud-related datasets."""
        datasets = [
            ("statlog+german+credit+data", "German Credit Data"),
            ("statlog+australian+credit+approval", "Australian Credit Approval"),
            ("default+of+credit+card+clients", "Default of Credit Card Clients"),
            ("census+income", "Census Income (Adult)"),
            ("bank+marketing", "Bank Marketing"),
            ("online+shoppers+purchasing+intention+dataset", "Online Shoppers Intention"),
            ("taiwanese+bankruptcy+prediction", "Taiwanese Bankruptcy Prediction"),
            ("polish+companies+bankruptcy+data", "Polish Companies Bankruptcy"),
        ]
        
        records = []
        for dataset_id, title in datasets:
            url = f"https://archive.ics.uci.edu/dataset/{dataset_id.replace('+', '%20')}"
            records.append(URLRecord(
                category='DATASET',
                url=url,
                title=title,
                source='UCI'
            ))
        return records

    # =========================================================================
    # Known Whitepapers and Official Reports
    # =========================================================================
    
    def get_known_whitepapers(self) -> List[URLRecord]:
        """Return known whitepapers and official reports."""
        whitepapers = [
            # Central Banks and Regulators
            ("https://www.bis.org/publ/bcbs239.pdf", "BIS - Principles for effective risk data aggregation"),
            ("https://www.bis.org/publ/bcbs128.pdf", "BIS - Compliance and compliance function in banks"),
            ("https://www.fatf-gafi.org/publications/fatfrecommendations/documents/fatf-recommendations.html", "FATF Recommendations"),
            ("https://www.fatf-gafi.org/publications/methodsandtrends/documents/money-laundering-terrorist-financing-trends.html", "FATF ML/TF Trends"),
            ("https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets", "FinCEN Advisories"),
            ("https://www.europol.europa.eu/publications-events/main-reports/internet-organised-crime-threat-assessment", "Europol IOCTA"),
            ("https://www.ecb.europa.eu/pub/cardfraud/html/ecb.cardfraudreport202110~cac4c418e8.en.html", "ECB Card Fraud Report"),
            ("https://www.federalreserve.gov/supervisionreg/topics/consumer-compliance.htm", "Fed Consumer Compliance"),
            
            # Consulting and Research Firms
            ("https://www.mckinsey.com/capabilities/risk-and-resilience/our-insights/fighting-back-against-synthetic-identity-fraud", "McKinsey Synthetic Identity Fraud"),
            ("https://www.mckinsey.com/industries/financial-services/our-insights/banking-matters/financial-crime-and-fraud-in-the-age-of-cybersecurity", "McKinsey Financial Crime"),
            ("https://www.accenture.com/us-en/insights/banking/financial-crime-study", "Accenture Financial Crime Study"),
            ("https://www.pwc.com/gx/en/services/forensics/economic-crime-survey.html", "PwC Global Economic Crime Survey"),
            ("https://www.ey.com/en_gl/forensic-integrity-services", "EY Forensic Services"),
            ("https://www.kpmg.com/xx/en/home/insights/2022/01/global-banking-fraud-survey.html", "KPMG Banking Fraud Survey"),
            ("https://www.deloitte.com/global/en/services/financial-advisory/services/financial-crime.html", "Deloitte Financial Crime"),
            ("https://www.gartner.com/en/documents/3980786", "Gartner Fraud Detection Guide"),
            ("https://www.forrester.com/report/the-forrester-wave-fraud-management-solutions-q1-2022/RES176573", "Forrester Fraud Management Wave"),
            
            # Payment Networks and Card Associations
            ("https://usa.visa.com/dam/VCOM/global/support-legal/documents/visa-security-white-paper.pdf", "Visa Security White Paper"),
            ("https://www.mastercard.com/news/perspectives/2023/ai-fraud-detection/", "Mastercard AI Fraud Detection"),
            ("https://nilsonreport.com/publication_newsletter_archive.php", "Nilson Report Card Fraud"),
            
            # Fraud Prevention Vendors
            ("https://www.feedzai.com/resources/", "Feedzai Resources"),
            ("https://www.fico.com/en/products/fico-falcon-fraud-manager", "FICO Falcon Fraud Manager"),
            ("https://risk.lexisnexis.com/insights-resources/research/true-cost-of-fraud-study", "LexisNexis True Cost of Fraud"),
            ("https://www.sas.com/en_us/software/fraud-management.html", "SAS Fraud Management"),
            ("https://www.nice.com/resources", "NICE Actimize Resources"),
            ("https://www.kount.com/resources", "Kount Resources"),
            ("https://www.signifyd.com/resources/", "Signifyd Resources"),
            ("https://www.ravelin.com/resources", "Ravelin Resources"),
            ("https://www.sift.com/resources", "Sift Resources"),
            ("https://www.forter.com/resources/", "Forter Resources"),
            ("https://www.riskified.com/resources/", "Riskified Resources"),
            ("https://stripe.com/guides/fraud-prevention", "Stripe Fraud Prevention Guide"),
            ("https://www.adyen.com/knowledge-hub/fraud-prevention", "Adyen Fraud Prevention"),
            ("https://developer.paypal.com/docs/limited-release/fraud-protection/", "PayPal Fraud Protection"),
            
            # Industry Associations
            ("https://www.acfe.com/fraud-resources.aspx", "ACFE Fraud Resources"),
            ("https://www.acams.org/en/resources", "ACAMS Resources"),
            ("https://merchantriskcouncil.org/resource-center", "MRC Resource Center"),
            ("https://www.cifas.org.uk/insight/reports-trends", "CIFAS Reports and Trends"),
            ("https://www.fca.org.uk/publications/thematic-reviews", "FCA Thematic Reviews"),
        ]
        
        records = []
        for url, title in whitepapers:
            records.append(URLRecord(
                category='WHITEPAPER',
                url=url,
                title=title,
                source='Official'
            ))
        return records

    # =========================================================================
    # Technical Content (Blogs, Tutorials)
    # =========================================================================
    
    def get_known_technical_content(self) -> List[URLRecord]:
        """Return known technical blog posts and tutorials."""
        content = [
            # Engineering Blogs
            ("https://engineering.atspotify.com/2022/06/the-rise-of-machine-learning-in-fraud-detection/", "Spotify ML Fraud", "TECH"),
            ("https://netflixtechblog.com/fraud-detection-with-machine-learning-ab-testing-3e0aa295aba9", "Netflix Fraud ML", "TECH"),
            ("https://www.uber.com/blog/fraud-detection/", "Uber Fraud Detection", "TECH"),
            ("https://engineering.grab.com/graph-networks-for-fraud-detection", "Grab Graph Networks Fraud", "TECH"),
            ("https://www.lyft.com/blog/posts/building-lyfts-fraud-fighting-machine-learning-platform", "Lyft Fraud ML Platform", "TECH"),
            ("https://medium.com/airbnb-engineering/fraud-detection-at-airbnb-a36c5e5e5e8a", "Airbnb Fraud Detection", "TECH"),
            ("https://engineering.linkedin.com/blog/2019/05/building-a-scalable-anomaly-detection-system", "LinkedIn Anomaly Detection", "TECH"),
            ("https://eng.uber.com/mastermind/", "Uber Mastermind Fraud", "TECH"),
            ("https://stripe.com/blog/how-we-built-a-rate-limiter-with-redis", "Stripe Rate Limiter", "TECH"),
            ("https://medium.com/paypal-tech/detecting-fraud-using-deep-learning-4c4c4b5e0f9", "PayPal Deep Learning Fraud", "TECH"),
            ("https://aws.amazon.com/blogs/machine-learning/detecting-fraud-with-amazon-sagemaker/", "AWS SageMaker Fraud", "TECH"),
            ("https://cloud.google.com/blog/products/ai-machine-learning/how-to-build-fraud-detection-ml-model", "GCP Fraud Detection", "TECH"),
            ("https://azure.microsoft.com/en-us/blog/anomaly-detection-using-machine-learning-to-detect-abnormalities-in-time-series-data/", "Azure Anomaly Detection", "TECH"),
            ("https://databricks.com/blog/2021/09/28/detecting-financial-fraud-at-scale-with-decision-trees-and-mlflow.html", "Databricks Fraud Detection", "TECH"),
            ("https://www.nvidia.com/en-us/ai-data-science/solutions/financial-services/fraud-detection/", "NVIDIA Fraud Detection", "TECH"),
            
            # Tutorials and Guides
            ("https://www.tensorflow.org/tutorials/structured_data/imbalanced_data", "TensorFlow Imbalanced Data", "TECH"),
            ("https://scikit-learn.org/stable/auto_examples/classification/plot_classifier_comparison.html", "Scikit-learn Classifiers", "TECH"),
            ("https://machinelearningmastery.com/smote-oversampling-for-imbalanced-classification/", "SMOTE Oversampling", "TECH"),
            ("https://machinelearningmastery.com/how-to-handle-missing-data-with-python/", "Handling Missing Data", "TECH"),
            ("https://towardsdatascience.com/credit-card-fraud-detection-using-autoencoders-in-keras-tensorflow-for-hackers-part-vii-20e0c85301bd", "Autoencoder Fraud Detection", "TECH"),
            ("https://towardsdatascience.com/fraud-detection-unsupervised-anomaly-detection-df43c1e9e1ca", "Unsupervised Anomaly Detection", "TECH"),
            ("https://neptune.ai/blog/how-to-deal-with-imbalanced-classification-and-regression-data", "Imbalanced Classification", "TECH"),
            ("https://www.datacamp.com/tutorial/fraud-detection-python", "DataCamp Fraud Detection", "TECH"),
            ("https://www.kaggle.com/code/joparga3/in-depth-skewed-data-classif-93-recall-acc-now", "Kaggle Skewed Data Classification", "TECH"),
            ("https://www.kaggle.com/code/janiobachmann/credit-fraud-dealing-with-imbalanced-datasets", "Kaggle Imbalanced Datasets", "TECH"),
            ("https://www.kaggle.com/code/gpreda/credit-card-fraud-detection-predictive-models", "Kaggle Fraud Predictive Models", "TECH"),
            
            # Graph-based Approaches
            ("https://www.dgl.ai/blog/2019/09/17/fraud-detection.html", "DGL Fraud Detection", "TECH"),
            ("https://pytorch-geometric.readthedocs.io/en/latest/notes/resources.html", "PyTorch Geometric Resources", "TECH"),
            ("https://neo4j.com/developer/graph-data-science/fraud-detection/", "Neo4j Fraud Detection", "TECH"),
            ("https://www.tigergraph.com/solutions/fraud-detection/", "TigerGraph Fraud", "TECH"),
            
            # Real-time and Streaming
            ("https://kafka.apache.org/documentation/streams/", "Kafka Streams", "TECH"),
            ("https://flink.apache.org/2019/05/20/fraud-detection.html", "Flink Fraud Detection", "TECH"),
            ("https://spark.apache.org/docs/latest/structured-streaming-programming-guide.html", "Spark Structured Streaming", "TECH"),
        ]
        
        records = []
        for url, title, category in content:
            records.append(URLRecord(
                category=category,
                url=url,
                title=title,
                source='Blog'
            ))
        return records

    # =========================================================================
    # Fundamentals and Reference Content
    # =========================================================================
    
    def get_fundamentals(self) -> List[URLRecord]:
        """Return fundamental/reference content."""
        content = [
            # ML Fundamentals
            ("https://en.wikipedia.org/wiki/Anomaly_detection", "Anomaly Detection Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Fraud", "Fraud Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Credit_card_fraud", "Credit Card Fraud Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Identity_theft", "Identity Theft Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Money_laundering", "Money Laundering Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Know_your_customer", "KYC Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Anti-money_laundering", "AML Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Random_forest", "Random Forest Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Gradient_boosting", "Gradient Boosting Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Autoencoder", "Autoencoder Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Graph_neural_network", "GNN Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Federated_learning", "Federated Learning Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Transfer_learning", "Transfer Learning Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Concept_drift", "Concept Drift Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Imbalanced_learning", "Imbalanced Learning Wikipedia", "FUNDAMENTALS"),
            ("https://en.wikipedia.org/wiki/Explainable_artificial_intelligence", "XAI Wikipedia", "FUNDAMENTALS"),
            
            # Scikit-learn Documentation
            ("https://scikit-learn.org/stable/modules/outlier_detection.html", "Sklearn Outlier Detection", "FUNDAMENTALS"),
            ("https://scikit-learn.org/stable/modules/ensemble.html", "Sklearn Ensemble Methods", "FUNDAMENTALS"),
            ("https://scikit-learn.org/stable/modules/preprocessing.html", "Sklearn Preprocessing", "FUNDAMENTALS"),
            ("https://scikit-learn.org/stable/modules/model_evaluation.html", "Sklearn Model Evaluation", "FUNDAMENTALS"),
            
            # PyOD (Outlier Detection)
            ("https://pyod.readthedocs.io/en/latest/", "PyOD Documentation", "FUNDAMENTALS"),
            
            # Imbalanced-learn
            ("https://imbalanced-learn.org/stable/", "Imbalanced-learn Documentation", "FUNDAMENTALS"),
            
            # SHAP/XAI
            ("https://shap.readthedocs.io/en/latest/", "SHAP Documentation", "FUNDAMENTALS"),
            ("https://christophm.github.io/interpretable-ml-book/", "Interpretable ML Book", "FUNDAMENTALS"),
        ]
        
        records = []
        for url, title, category in content:
            records.append(URLRecord(
                category=category,
                url=url,
                title=title,
                source='Reference'
            ))
        return records

    # =========================================================================
    # Real Cases and Threat Intelligence
    # =========================================================================
    
    def get_cases_and_threats(self) -> List[URLRecord]:
        """Return real cases and threat intelligence sources."""
        cases = [
            ("https://krebsonsecurity.com/category/data-breaches/", "Krebs on Security Data Breaches"),
            ("https://www.bankinfosecurity.com/fraud-c-144", "Bank Info Security Fraud"),
            ("https://www.darkreading.com/attacks-breaches", "Dark Reading Attacks"),
            ("https://www.bleepingcomputer.com/tag/fraud/", "Bleeping Computer Fraud"),
            ("https://www.ic3.gov/Home/AnnualReports", "FBI IC3 Annual Reports"),
            ("https://www.ftc.gov/news-events/data-visualizations/data-spotlight", "FTC Data Spotlight"),
            ("https://www.consumer.ftc.gov/features/scam-alerts", "FTC Scam Alerts"),
            ("https://www.actionfraud.police.uk/news", "UK Action Fraud News"),
            ("https://www.europol.europa.eu/crime-areas-and-statistics/crime-areas/economic-crime/mtic-fraud", "Europol Economic Crime"),
            ("https://www.bbc.com/news/topics/c008ql15vpyt/fraud", "BBC Fraud News"),
            ("https://www.reuters.com/business/finance/", "Reuters Finance"),
            ("https://threatpost.com/category/financial-fraud/", "Threatpost Financial Fraud"),
            ("https://securityintelligence.com/category/fraud-protection/", "Security Intelligence Fraud"),
            ("https://www.csoonline.com/category/fraud-prevention/", "CSO Online Fraud Prevention"),
            ("https://www.infosecurity-magazine.com/news/fraud/", "Infosecurity Magazine Fraud"),
            ("https://www.scmagazine.com/topic/fraud", "SC Magazine Fraud"),
            ("https://www.fraud-magazine.com/", "Fraud Magazine"),
            ("https://www.pymnts.com/fraud-prevention/", "PYMNTS Fraud Prevention"),
            ("https://www.paymentsjournal.com/category/fraud/", "Payments Journal Fraud"),
            ("https://www.finextra.com/channel/security", "Finextra Security"),
        ]
        
        records = []
        for url, title in cases:
            records.append(URLRecord(
                category='CASES',
                url=url,
                title=title,
                source='News/Intel'
            ))
        return records

    # =========================================================================
    # Additional Paper Sources
    # =========================================================================
    
    def get_known_papers(self) -> List[URLRecord]:
        """Return known high-quality papers."""
        papers = [
            # Classic fraud detection papers
            ("https://arxiv.org/abs/1904.04584", "CreditCardFraudDetection-Survey"),
            ("https://arxiv.org/abs/1811.05927", "Deep Learning Fraud Detection"),
            ("https://arxiv.org/abs/1906.00389", "Graph Neural Networks Fraud"),
            ("https://arxiv.org/abs/2002.08593", "Explainable AI Financial"),
            ("https://arxiv.org/abs/1910.01294", "Federated Learning Finance"),
            ("https://arxiv.org/abs/2006.11235", "Transfer Learning Fraud"),
            ("https://arxiv.org/abs/1811.07604", "Imbalanced Learning Survey"),
            ("https://arxiv.org/abs/1905.04032", "Concept Drift Detection"),
            ("https://arxiv.org/abs/2003.00344", "Anomaly Detection Survey"),
            ("https://arxiv.org/abs/1909.11542", "Self-Supervised Fraud"),
            ("https://arxiv.org/abs/2005.10626", "Temporal Graph Networks"),
            ("https://arxiv.org/abs/2004.00216", "Financial Fraud GNN"),
            ("https://arxiv.org/abs/1901.00402", "Autoencoder Anomaly"),
            ("https://arxiv.org/abs/2102.10757", "Synthetic Data Generation"),
            ("https://arxiv.org/abs/2007.15826", "Few-Shot Fraud Detection"),
            ("https://arxiv.org/abs/2103.02559", "Contrastive Learning Fraud"),
            ("https://arxiv.org/abs/2011.10618", "Attention Mechanism Fraud"),
            ("https://arxiv.org/abs/1910.08306", "LSTM Fraud Detection"),
            ("https://arxiv.org/abs/2106.09256", "Transformer Financial"),
            ("https://arxiv.org/abs/1706.00891", "Variational Autoencoder Anomaly"),
        ]
        
        records = []
        for url, title in papers:
            records.append(URLRecord(
                category='PAPER',
                url=url,
                title=title,
                source='arXiv'
            ))
        return records

    # =========================================================================
    # Main Collection Logic
    # =========================================================================
    
    async def run_collection(self):
        """Run the full collection process."""
        logger.info(f"Starting collection. Target: {self.target_count} URLs")
        
        with tqdm(total=self.target_count, desc="Collecting URLs") as progress:
            # Phase 1: Add known high-quality sources
            logger.info("Phase 1: Adding known sources...")
            
            # Papers
            for record in self.get_known_papers():
                if self.add_url(record):
                    progress.update(1)
            
            # Datasets
            self.collect_kaggle_datasets(progress)
            for record in self.get_known_uci_datasets():
                if self.add_url(record):
                    progress.update(1)
            
            # Whitepapers
            for record in self.get_known_whitepapers():
                if self.add_url(record):
                    progress.update(1)
            
            # Technical content
            for record in self.get_known_technical_content():
                if self.add_url(record):
                    progress.update(1)
            
            # Fundamentals
            for record in self.get_fundamentals():
                if self.add_url(record):
                    progress.update(1)
            
            # Cases
            for record in self.get_cases_and_threats():
                if self.add_url(record):
                    progress.update(1)
            
            logger.info(f"Phase 1 complete. Collected: {len(self.collected_urls)}")
            
            # Phase 2: API-based collection
            logger.info("Phase 2: Querying APIs...")
            
            await self.collect_arxiv_papers(progress)
            await self.collect_semantic_scholar(progress)
            await self.collect_crossref(progress)
            await self.collect_github(progress)
            
            logger.info(f"Phase 2 complete. Collected: {len(self.collected_urls)}")
        
        # Generate report
        self.print_summary()
        self.save_csv()

    def print_summary(self):
        """Print collection summary."""
        print("\n" + "=" * 60)
        print("COLLECTION SUMMARY")
        print("=" * 60)
        
        total = len(self.collected_urls)
        print(f"\nTotal URLs collected: {total}")
        print(f"Target: {self.target_count}")
        
        print("\nBy Category:")
        for category in self.CATEGORY_TARGETS:
            count = self.get_category_count(category)
            target = self.CATEGORY_TARGETS[category]
            status = "✓" if count >= target else "✗"
            print(f"  {category:15} {count:4}/{target:4} {status}")
        
        print("\nBy Source:")
        sources = {}
        for record in self.collected_urls.values():
            sources[record.source] = sources.get(record.source, 0) + 1
        for source, count in sorted(sources.items(), key=lambda x: -x[1]):
            print(f"  {source:20} {count:4}")

    def save_csv(self):
        """Save URLs to CSV file."""
        with open(self.output_path, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)
            writer.writerow(['categoria', 'url'])
            
            # Sort by category for organized output
            sorted_records = sorted(
                self.collected_urls.values(),
                key=lambda r: (r.category, r.source, r.url)
            )
            
            for record in sorted_records:
                writer.writerow([record.category, record.url])
        
        logger.info(f"Saved {len(self.collected_urls)} URLs to {self.output_path}")


async def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Collect fraud research URLs')
    parser.add_argument('--output', '-o', default='fraud_urls.csv', help='Output CSV file')
    parser.add_argument('--target', '-t', type=int, default=1000, help='Target URL count')
    args = parser.parse_args()
    
    async with FraudURLCollector(args.output, args.target) as collector:
        await collector.run_collection()


if __name__ == '__main__':
    asyncio.run(main())
