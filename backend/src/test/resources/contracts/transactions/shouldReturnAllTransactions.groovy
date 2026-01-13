import org.springframework.cloud.contract.spec.Contract

Contract.make {
    name "should_return_all_transactions"
    description "Returns paginated list of transactions"
    
    request {
        method GET()
        url "/api/transactions"
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
            content: $(anyList()),
            pageable: [
                pageNumber: $(anyNumber()),
                pageSize: $(anyNumber())
            ],
            totalElements: $(anyNumber()),
            totalPages: $(anyNumber()),
            last: $(anyBoolean()),
            first: $(anyBoolean()),
            empty: $(anyBoolean())
        ])
    }
}
