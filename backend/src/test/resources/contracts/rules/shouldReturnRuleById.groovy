import org.springframework.cloud.contract.spec.Contract

Contract.make {
    name "should_return_rule_by_id"
    description "Returns a specific rule by its ID"
    
    request {
        method GET()
        urlPath("/api/rules/1")
        headers {
            header("Authorization", "Basic YWRtaW46YWRtaW4xMjM=") // admin:admin123
            contentType(applicationJson())
        }
    }
    
    response {
        status OK()
        headers {
            contentType(applicationJson())
        }
        body([
            id: 1,
            name: $(anyNonBlankString()),
            description: $(anyNonBlankString()),
            type: $(anyOf("VELOCITY", "SECURITY", "ANOMALY", "BLOCKLIST", "GEO", "DEVICE")),
            status: $(anyOf("ACTIVE", "INACTIVE")),
            weight: $(anyNumber()),
            version: $(anyNumber()),
            createdAt: $(anyNonBlankString()),
            updatedAt: $(anyNonBlankString())
        ])
    }
}
