import { Filter, Search, ArrowRight } from 'lucide-react';

import { Button } from '@/components/ui/button';
import { Card, CardContent } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';

import type { RuleTypeFilter } from '@/pages/rules/useUnifiedRules';

type RulesFiltersProps = {
  searchTerm: string;
  onSearchTermChange: (value: string) => void;
  ruleTypeFilter: RuleTypeFilter;
  onRuleTypeFilterChange: (value: RuleTypeFilter) => void;
  totalCount: number;
  simpleCount: number;
  complexCount: number;
  onGoToComplexRules: () => void;
};

export function RulesFilters({
  searchTerm,
  onSearchTermChange,
  ruleTypeFilter,
  onRuleTypeFilterChange,
  totalCount,
  simpleCount,
  complexCount,
  onGoToComplexRules,
}: RulesFiltersProps) {
  return (
    <>
      <div className="flex flex-wrap gap-4 items-center">
        <div className="relative flex-1 min-w-[200px] max-w-md">
          <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
          <Input
            placeholder="Buscar regras..."
            value={searchTerm}
            onChange={(e) => onSearchTermChange(e.target.value)}
            className="pl-10"
          />
        </div>
        <Select value={ruleTypeFilter} onValueChange={(v) => onRuleTypeFilterChange(v as RuleTypeFilter)}>
          <SelectTrigger className="w-[180px]">
            <Filter className="h-4 w-4 mr-2" />
            <SelectValue placeholder="Filtrar por tipo" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">Todas ({totalCount})</SelectItem>
            <SelectItem value="simple">Simples ({simpleCount})</SelectItem>
            <SelectItem value="complex">Complexas ({complexCount})</SelectItem>
          </SelectContent>
        </Select>
      </div>
      <Card className="border border-dashed">
        <CardContent className="flex flex-col gap-3 py-4 sm:flex-row sm:items-center sm:justify-between">
          <div>
            <p className="text-sm font-medium text-foreground">Precisa de regras avançadas?</p>
            <p className="text-xs text-muted-foreground">
              Use o construtor de regras complexas com grupos, condições aninhadas e templates.
            </p>
          </div>
          <Button onClick={onGoToComplexRules} variant="secondary">
            Ir para Regras Complexas
            <ArrowRight className="ml-2 h-4 w-4" />
          </Button>
        </CardContent>
      </Card>
    </>
  );
}
