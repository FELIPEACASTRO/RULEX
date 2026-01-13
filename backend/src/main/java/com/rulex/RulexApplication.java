package com.rulex;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableAsync;

/**
 * Aplicação principal do RULEX - Sistema de Regras Duras para Transações de Crédito.
 *
 * <p>Esta aplicação implementa um motor de regras configurável para análise e prevenção de fraudes
 * em transações de crédito, com suporte a auditoria completa e compliance.
 */
@SpringBootApplication
@ComponentScan(basePackages = "com.rulex")
@EnableAsync
public class RulexApplication {

  public static void main(String[] args) {
    SpringApplication.run(RulexApplication.class, args);
  }
}
