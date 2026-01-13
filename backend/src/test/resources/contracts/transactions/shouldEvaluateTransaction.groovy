import org.springframework.cloud.contract.spec.Contract

Contract.make {
    name "should_evaluate_transaction"
    description "Evaluates a transaction against fraud rules"
    
    request {
        method POST()
        url "/api/transactions/evaluate"
        headers {
            header("Authorization", "Basic YWRtaW46YWRtaW4xMjM=") // admin:admin123
            contentType(applicationJson())
        }
        body([
            transactionId: "TXN-CONTRACT-TEST-001",
            amount: 1500.00,
            currency: "BRL",
            merchantId: "MERCHANT-001",
            merchantCategory: "5411",
            customerId: "CUSTOMER-001",
            cardNumberHash: "abc123def456",
            channel: "ECOMMERCE",
            timestamp: "2025-01-15T10:30:00Z",
            location: [
                country: "BR",
                city: "São Paulo",
                latitude: -23.5505,
                longitude: -46.6333
            ],
            device: [
                fingerprint: "device-fp-001",
                type: "MOBILE",
                os: "Android 14",
                browser: "Chrome Mobile"
            ]
        ])
    }
    
    response {
        status OK()
        headers {
            contentType(applicationJson())
        }
        body([
            transactionId: "TXN-CONTRACT-TEST-001",
            decision: $(anyOf("APPROVED", "BLOCKED", "REVIEW", "CHALLENGE")),
            score: $(anyNumber()),
            triggeredRules: $(anyList()),
            evaluationTimeMs: $(anyNumber()),
            timestamp: $(anyNonBlankString())
        ])
    }
}
