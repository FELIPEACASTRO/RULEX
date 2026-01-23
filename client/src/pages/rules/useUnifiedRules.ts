import { useMemo } from 'react';

import { ComplexRuleDTO, RuleConfiguration } from '@/lib/javaApi';

export type RuleTypeFilter = 'all' | 'simple' | 'complex';

export type UnifiedRule = {
  id: string | number;
  name: string;
  description: string;
  type: 'simple' | 'complex';
  ruleType?: string;
  classification?: string;
  decision?: ComplexRuleDTO['decision'];
  enabled: boolean;
  weight?: number;
  priority?: number;
  threshold?: number;
  severity?: number;
  conditionsCount: number;
  complexId?: string;
  complexKey?: string;
  original: RuleConfiguration | ComplexRuleDTO;
};

type UnifiedRuleParams = {
  simpleRules: RuleConfiguration[];
  complexRules: ComplexRuleDTO[];
  ruleTypeFilter: RuleTypeFilter;
  searchTerm: string;
};

export const useUnifiedRules = ({
  simpleRules,
  complexRules,
  ruleTypeFilter,
  searchTerm,
}: UnifiedRuleParams) => {
  const unifiedRules = useMemo((): UnifiedRule[] => {
    const simple: UnifiedRule[] = simpleRules.map((rule) => ({
      id: rule.id,
      name: rule.ruleName,
      description: rule.description ?? '',
      type: 'simple',
      ruleType: rule.ruleType,
      classification: rule.classification,
      enabled: rule.enabled,
      weight: rule.weight,
      threshold: rule.threshold,
      conditionsCount: rule.conditions?.length ?? 0,
      original: rule,
    }));

    const complex: UnifiedRule[] = complexRules.map((rule) => ({
      id: rule.id ?? rule.key,
      name: rule.title || rule.key,
      description: rule.description ?? '',
      type: 'complex',
      decision: rule.decision,
      enabled: rule.enabled,
      priority: rule.priority,
      severity: rule.severity,
      conditionsCount: rule.rootConditionGroup?.conditions?.length ?? 0,
      complexId: rule.id,
      complexKey: rule.key,
      original: rule,
    }));

    return [...simple, ...complex];
  }, [simpleRules, complexRules]);

  const filteredRules = useMemo(() => {
    return unifiedRules.filter((rule) => {
      if (ruleTypeFilter !== 'all' && rule.type !== ruleTypeFilter) {
        return false;
      }

      if (searchTerm) {
        const search = searchTerm.toLowerCase();
        return (
          rule.name.toLowerCase().includes(search) ||
          rule.description.toLowerCase().includes(search)
        );
      }

      return true;
    });
  }, [unifiedRules, ruleTypeFilter, searchTerm]);

  return { unifiedRules, filteredRules };
};
