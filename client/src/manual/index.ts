/**
 * Manual Components Index
 * Exporta todos os componentes do Manual para fácil importação
 */

// Catálogos de dados
export { ActionsCatalog } from './ActionsCatalog';
export { FunctionsCatalog } from './FunctionsCatalog';
export { ApiCatalog } from './ApiCatalog';
export { DbCatalog } from './DbCatalog';

// Guias e mapas
export { SystemMap } from './SystemMap';
export { QaAndE2EGuide } from './QaAndE2EGuide';
export { InfraRunbook } from './InfraRunbook';
export { ComplexRulesGuide } from './ComplexRulesGuide';

// Biblioteca de Regras de Exemplo
export { RulesLibrary, RULES_LIBRARY, RULES_LIBRARY_STATS } from './RulesLibrary';
export type { RuleExample, RuleComplexity, RuleCategory, ExamplePayload } from './RulesLibrary';

// Re-export generated data
export * from './generated';
