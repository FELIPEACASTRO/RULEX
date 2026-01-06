/**
 * Domain Services - RULEX Fraud Detection Engine.
 *
 * <p>Este pacote contém os serviços de domínio puros sem dependências de framework. Implementam
 * regras de negócio que não dependem de infraestrutura.
 *
 * <ul>
 *   <li><b>ConditionEvaluator</b> - Avalia condições individuais (operadores)
 *   <li><b>RuleEvaluatorService</b> - Avalia regras contra payload
 *   <li><b>TamperDetector</b> - Detecta adulteração de payload
 *   <li><b>ScoreCalculator</b> - Calcula e normaliza scores de risco
 * </ul>
 *
 * <p><b>Regras:</b>
 *
 * <ul>
 *   <li>Nenhuma anotação Spring (@Service, @Component, etc.)
 *   <li>Nenhum acesso a banco de dados ou cache
 *   <li>Nenhuma chamada HTTP ou serviço externo
 *   <li>Podem ser instanciados com 'new' (sem container)
 *   <li>100% testáveis sem mocks
 * </ul>
 *
 * @see com.rulex.application.usecase Para orquestração com infraestrutura
 */
package com.rulex.domain.service;
