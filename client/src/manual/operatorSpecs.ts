export type OperatorDocLevel = "manual" | "spec" | "generated";
export type OperatorDocConfidence = "high" | "medium" | "low";

export type OperatorSpec = {
  name: string;
  summary?: string;
  syntax?: string;
  syntaxExplanation?: string;
  story?: string;
  problem?: string;
  analogy?: string;
  stepByStep?: string[];
  before?: string;
  after?: string;
  commonQuestion?: string;
  commonAnswer?: string;
  goldenTip?: string;

  // Advanced (optional)
  engineBehavior?: {
    description: string;
    steps: string[];
    performance?: string;
    cautions?: string[];
  };
  realScenarios?: Array<{
    title: string;
    context: string;
    problem: string;
    solution: string;
    impact: string;
  }>;
  possibleOutcomes?: {
    whenTrue: string;
    whenFalse: string;
    recommendedAction?: string;
  };
  howToTest?: string[];
};

//
// IMPORTANT:
// The backend operator list currently ships with empty comments/categories.
// This file is the single place to add authoritative, non-heuristic docs.
//
// Add entries like:
// export const OPERATOR_SPECS: Record<string, OperatorSpec> = {
//   SOME_OPERATOR: { name: "SOME_OPERATOR", summary: "...", syntax: "..." }
// };
//
export const OPERATOR_SPECS: Record<string, OperatorSpec> = {};
