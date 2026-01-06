/**
 * Application Ports (IN) - RULEX Fraud Detection Engine.
 *
 * <p>Este pacote contém as interfaces (Ports) que definem como a camada de infraestrutura (ex:
 * Controllers) invoca a camada de aplicação.
 *
 * <h2>Ports Disponíveis:</h2>
 *
 * <ul>
 *   <li><b>AnalyzeTransactionUseCase</b> - Análise de fraude em transações
 *   <li><b>ManageRulesUseCase</b> - CRUD de regras de fraude
 * </ul>
 *
 * <h2>Padrão Hexagonal:</h2>
 *
 * <pre>
 *     ┌────────────────────────────────────────────┐
 *     │         Infrastructure Layer               │
 *     │                                            │
 *     │   ┌────────────────────────────────────┐   │
 *     │   │  Controllers (REST)                │   │
 *     │   └───────────────┬────────────────────┘   │
 *     └───────────────────┼────────────────────────┘
 *                         │
 *     ┌───────────────────▼────────────────────────┐
 *     │           Application Layer                │
 *     │                                            │
 *     │   ┌───────────────┐     ┌──────────────┐   │
 *     │   │ Ports (IN)    │────▶│  Use Cases   │   │
 *     │   └───────────────┘     └──────────────┘   │
 *     └────────────────────────────────────────────┘
 * </pre>
 *
 * <p><b>Regras:</b>
 *
 * <ul>
 *   <li>Interfaces usam apenas Domain Models
 *   <li>Commands validam entrada no construtor
 *   <li>Controllers injetam Ports, não Use Cases concretos
 * </ul>
 *
 * @see com.rulex.application.usecase Para implementações
 */
package com.rulex.application.port.in;
