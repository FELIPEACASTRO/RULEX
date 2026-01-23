/**
 * Manual Components Index
 * Re-exports all manual components for clean imports
 */

// Catalog components
export { ActionsCatalog } from "./ActionsCatalog";
export { FunctionsCatalog } from "./FunctionsCatalog";
export { ApiCatalog } from "./ApiCatalog";
export { DbCatalog } from "./DbCatalog";
export { SystemMap } from "./SystemMap";
export { QaAndE2EGuide } from "./QaAndE2EGuide";
export { InfraRunbook } from "./InfraRunbook";
export { ComplexRulesGuide } from "./ComplexRulesGuide";
export { RulesLibrary } from "./RulesLibrary";
export { OperatorCatalog } from "./OperatorCatalog";
export { FieldDictionary } from "./FieldDictionary";
export { TemplatesGallery } from "./TemplatesGallery";

// Data exports
export * from "./manualData";
export { RULES_LIBRARY_STATS } from "./data/rulesLibraryStats";

// NOTE: generated artifacts are imported from "@/manual/generated" directly.
// Re-exporting here causes name collisions (e.g. LogicOperator).
