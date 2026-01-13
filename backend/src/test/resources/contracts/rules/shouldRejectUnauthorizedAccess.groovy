import org.springframework.cloud.contract.spec.Contract

Contract.make {
    name "should_reject_unauthorized_access"
    description "Returns 401 when no authorization header is provided"
    
    request {
        method GET()
        url "/api/rules"
        headers {
            contentType(applicationJson())
        }
    }
    
    response {
        status UNAUTHORIZED()
    }
}
