import org.springframework.cloud.contract.spec.Contract

Contract.make {
    name "should_return_all_rules"
    description "Returns a list of all fraud detection rules"
    
    request {
        method GET()
        url "/api/rules"
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
            [
                id: $(anyNumber()),
                name: $(anyNonBlankString()),
                description: $(anyNonBlankString()),
                type: $(anyOf("VELOCITY", "SECURITY", "ANOMALY", "BLOCKLIST", "GEO", "DEVICE")),
                status: $(anyOf("ACTIVE", "INACTIVE")),
                weight: $(anyNumber()),
                version: $(anyNumber()),
                createdAt: $(anyNonBlankString()),
                updatedAt: $(anyNonBlankString())
            ]
        ])
    }
}
