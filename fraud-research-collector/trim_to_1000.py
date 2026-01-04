#!/usr/bin/env python3
"""Trim CSV to exactly 1000 URLs while maintaining category balance."""

import csv
from collections import defaultdict

# Target counts per category (total = 1000)
TARGETS = {
    'PAPER': 300,
    'DATASET': 200,
    'WHITEPAPER': 150,
    'TECH': 150,
    'CASES': 100,
    'FUNDAMENTALS': 100,
}

def main():
    # Read all URLs
    urls_by_category = defaultdict(list)
    
    with open('fraud_urls_1000.csv', 'r', encoding='utf-8') as f:
        reader = csv.reader(f)
        header = next(reader)
        for row in reader:
            if len(row) >= 2:
                category, url = row[0], row[1]
                urls_by_category[category].append(url)
    
    # Trim each category to target
    final_urls = []
    for category, target in TARGETS.items():
        urls = urls_by_category[category][:target]
        for url in urls:
            final_urls.append((category, url))
    
    # Sort by category
    final_urls.sort(key=lambda x: (x[0], x[1]))
    
    # Save exactly 1000 URLs
    with open('fraud_urls_FINAL_1000.csv', 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(['categoria', 'url'])
        for category, url in final_urls:
            writer.writerow([category, url])
    
    # Print summary
    print(f"✅ Created fraud_urls_FINAL_1000.csv with exactly {len(final_urls)} URLs")
    
    counts = defaultdict(int)
    for cat, _ in final_urls:
        counts[cat] += 1
    
    for cat, count in sorted(counts.items()):
        print(f"  {cat}: {count}")

if __name__ == '__main__':
    main()
