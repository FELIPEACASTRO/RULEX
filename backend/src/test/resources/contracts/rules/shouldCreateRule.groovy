import org.springframework.cloud.contract.spec.Contract

Contract.make {
    name "should_create_rule"
    description "Creates a new fraud detection rule"
    
    request {
        method POST()
        url "/api/rules"
        headers {
            header("Authorization", "Basic YWRtaW46YWRtaW4xMjM=") // admin:admin123
            contentType(applicationJson())
        }
        body([
            name: "TEST_VELOCITY_RULE",
            description: "Test velocity rule for contract testing",
            type: "VELOCITY",
            status: "ACTIVE",
            conditions: [
                [
                    field: "velocity.tx_count_1h",
                    operator: "GREATER_THAN",
                    value: "10"
                ]
            ],
            action: "REVIEW",
            weight: 50,
            classification: "TEST_CLASSIFICATION"
        ])
    }
    
    response {
        status CREATED()
        headers {
            contentType(applicationJson())
        }
        body([
            id: $(anyNumber()),
            name: "TEST_VELOCITY_RULE",
            description: "Test velocity rule for contract testing",
            type: "VELOCITY",
            status: "ACTIVE",
            weight: 50,
            version: 0,
            createdAt: $(anyNonBlankString()),
            updatedAt: $(anyNonBlankString())
        ])
    }
}
