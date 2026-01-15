import {
  CLASSIFICATIONS,
  FIELD_REF_OPERATORS,
  LOGIC_OPERATORS as SIMPLE_LOGIC_OPERATORS,
  OPERATORS as SIMPLE_OPERATORS,
  OPERATORS_BY_TYPE,
  RULE_TYPES,
  UNARY_OPERATORS,
} from "@/components/RuleFormDialog/types";

import {
  COMPARISON_OPERATORS,
  LOGIC_OPERATORS as COMPLEX_LOGIC_OPERATORS,
  VALUE_TYPES,
} from "@/components/ComplexRuleBuilder/types";

export type ManualSection = {
  id: string;
  title: string;
  description: string;
};

export const MANUAL_SECTIONS: ManualSection[] = [
  {
    id: "visao-geral",
    title: "Visao geral",
    description:
      "Conteudo gerado a partir das listas e tipos existentes no codigo do frontend.",
  },
  {
    id: "regras-form",
    title: "Regras (RuleFormDialog)",
    description: "Listas usadas pelo formulario de regras e seus tipos.",
  },
  {
    id: "regras-complexas",
    title: "Regras complexas (ComplexRuleBuilder)",
    description: "Operadores e tipos usados no construtor visual de regras complexas.",
  },
];

export const MANUAL_DATA = {
  generatedFrom: {
    ruleFormDialog: {
      ruleTypes: RULE_TYPES,
      classifications: CLASSIFICATIONS,
      logicOperators: SIMPLE_LOGIC_OPERATORS,
      operators: SIMPLE_OPERATORS,
      unaryOperators: UNARY_OPERATORS,
      fieldRefOperators: FIELD_REF_OPERATORS,
      operatorsByType: OPERATORS_BY_TYPE,
    },
    complexRuleBuilder: {
      logicOperators: COMPLEX_LOGIC_OPERATORS,
      comparisonOperators: COMPARISON_OPERATORS,
      valueTypes: VALUE_TYPES,
    },
  },
} as const;
