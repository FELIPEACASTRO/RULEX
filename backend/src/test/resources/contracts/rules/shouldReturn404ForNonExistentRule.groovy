import org.springframework.cloud.contract.spec.Contract

Contract.make {
    name "should_return_404_for_non_existent_rule"
    description "Returns 404 when rule is not found"
    
    request {
        method GET()
        urlPath("/api/rules/999999")
        headers {
            header("Authorization", "Basic YWRtaW46YWRtaW4xMjM=") // admin:admin123
            contentType(applicationJson())
        }
    }
    
    response {
        status NOT_FOUND()
        headers {
            contentType(applicationJson())
        }
        body([
            timestamp: $(anyNonBlankString()),
            status: 404,
            error: "Not Found",
            message: $(anyNonBlankString()),
            path: "/api/rules/999999"
        ])
    }
}
