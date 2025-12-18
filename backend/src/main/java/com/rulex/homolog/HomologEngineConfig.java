package com.rulex.homolog;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class HomologEngineConfig {

    @Bean
    public SafeRuleDslEvaluator safeRuleDslEvaluator(ObjectMapper objectMapper) {
        return new SafeRuleDslEvaluator(objectMapper);
    }
}
