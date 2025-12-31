# RULE_MODAL_GAPS.md - Gaps Priorizados do Modal de Regras

**Data:** 2024-12-31
**Versão:** 1.0.0
**Status:** 🟡 P0 COMPLETO - P1/P2 PARCIAL

---

## RESUMO EXECUTIVO

| Prioridade | Total | Resolvidos | Pendentes |
|------------|-------|------------|-----------|
| **P0 (Crítico)** | 5 | 5 | 0 |
| **P1 (Importante)** | 10 | 6 | 4 |
| **P2 (Polimento)** | 8 | 0 | 8 |
| **TOTAL** | 23 | 11 | 12 |

---

## P0 - CRÍTICOS (Quebra funcional / Regra inválida / Perda de dados)

### P0-01: Validação de REGEX inválida no operador MATCHES_REGEX

**Descrição:** Usuário pode salvar uma regra com regex inválida (ex: `[invalid`), causando erro no backend durante avaliação.

**Impacto:** Regra salva mas não funciona; erro silencioso em produção.

**Solução:**
```typescript
// Adicionar validação no schema.ts
if (operator === 'MATCHES_REGEX') {
  try {
    new RegExp(value);
  } catch {
    return { valid: false, message: 'Expressão regular inválida' };
  }
}
```

**Status:** ✅ Resolvido (schema.ts - refine com validação de RegExp)

---

### P0-02: Validação de formato BETWEEN

**Descrição:** Operador BETWEEN requer 2 valores (min,max ou min..max), mas não há validação.

**Impacto:** Erro no backend ou comportamento inesperado.

**Solução:**
```typescript
if (operator === 'BETWEEN' || operator === 'NOT_BETWEEN') {
  const parts = value.includes('..') ? value.split('..') : value.split(',');
  if (parts.length !== 2) {
    return { valid: false, message: 'Use o formato: min,max ou min..max' };
  }
  if (fieldType === 'number' && parts.some(p => isNaN(Number(p.trim())))) {
    return { valid: false, message: 'Ambos os valores devem ser números' };
  }
}
```

**Status:** ✅ Resolvido (schema.ts - refine com validação de 2 partes)

---

### P0-03: Validação de lista IN/NOT_IN

**Descrição:** Operadores IN/NOT_IN esperam lista, mas não há validação de formato.

**Impacto:** Erro de parsing ou comportamento incorreto.

**Solução:**
```typescript
if (operator === 'IN' || operator === 'NOT_IN') {
  // Aceitar: [1,2,3] ou 1,2,3 ou ["a","b"]
  const cleanValue = value.trim();
  if (cleanValue.startsWith('[') && !cleanValue.endsWith(']')) {
    return { valid: false, message: 'Lista deve terminar com ]' };
  }
  // Validar que tem pelo menos 1 item
  const items = cleanValue.replace(/[\[\]]/g, '').split(',').filter(Boolean);
  if (items.length === 0) {
    return { valid: false, message: 'Lista deve ter pelo menos 1 item' };
  }
}
```

**Status:** ✅ Resolvido (schema.ts - refine com validação de lista)

---

### P0-04: Warning de unsaved changes

**Descrição:** Usuário pode fechar o modal com alterações não salvas sem aviso.

**Impacto:** Perda de dados do usuário.

**Solução:**
```typescript
// 1. Adicionar estado isDirty
const [isDirty, setIsDirty] = useState(false);

// 2. Interceptar fechamento
const handleOpenChange = (open: boolean) => {
  if (!open && isDirty) {
    if (!confirm('Você tem alterações não salvas. Deseja sair?')) {
      return;
    }
  }
  setShowDialog(open);
};

// 3. Adicionar beforeunload
useEffect(() => {
  const handler = (e: BeforeUnloadEvent) => {
    if (isDirty) {
      e.preventDefault();
      e.returnValue = '';
    }
  };
  window.addEventListener('beforeunload', handler);
  return () => window.removeEventListener('beforeunload', handler);
}, [isDirty]);
```

**Status:** ✅ Resolvido (Rules.tsx - AlertDialog + isDirty state)

---

### P0-05: Conflito de versão (optimistic locking)

**Descrição:** Se dois usuários editam a mesma regra, o segundo sobrescreve sem aviso.

**Impacto:** Perda de alterações de outro usuário.

**Solução:**
```typescript
// 1. Enviar version no PUT
const payload = { ...data, version: editingRule.version };

// 2. Tratar 409 Conflict
onError: (error: Error) => {
  if (error.message.includes('409') || error.message.includes('conflict')) {
    toast.error('Esta regra foi modificada por outro usuário. Recarregue e tente novamente.');
    queryClient.invalidateQueries({ queryKey: ['rules'] });
    return;
  }
  toast.error(`Erro ao atualizar: ${error.message}`);
}
```

**Status:** ✅ Resolvido (Rules.tsx - version no payload + tratamento 409)

---

## P1 - IMPORTANTES (UX ruim / Validação incompleta / Acessibilidade)

### P1-01: Validação de ruleName em tempo real

**Descrição:** Validação só ocorre no submit, não durante digitação.

**Solução:** Usar react-hook-form com mode: 'onChange' e exibir erro inline.

**Status:** ✅ Resolvido (Rules.tsx - validateForm + validationErrors state)

---

### P1-02: Feedback visual de campo inválido

**Descrição:** Campos inválidos não têm borda vermelha ou mensagem de erro.

**Solução:** Adicionar classes condicionais e FormMessage do shadcn/ui.

**Status:** ✅ Resolvido (Rules.tsx - aria-invalid + border-red-500 + error messages)

---

### P1-03: Limite de condições

**Descrição:** Usuário pode adicionar centenas de condições, degradando performance.

**Solução:** Limitar a 20 condições com mensagem explicativa.

**Status:** ✅ Resolvido (schema.ts - MAX_CONDITIONS = 20)

---

### P1-04: Filtro de operadores por tipo de campo

**Descrição:** Operadores de string (CONTAINS, REGEX) aparecem para campos numéricos.

**Solução:** Usar OPERATORS_BY_TYPE já definido em types.ts.

**Status:** ❌ Pendente

---

### P1-05: Focus trap no modal

**Descrição:** Tab pode sair do modal para elementos atrás.

**Solução:** Usar FocusTrap do Radix ou implementar manualmente.

**Status:** ❌ Pendente

---

### P1-06: ARIA labels adequados

**Descrição:** Inputs sem aria-label ou aria-describedby para erros.

**Solução:** Adicionar aria-invalid, aria-describedby para mensagens de erro.

**Status:** ❌ Pendente

---

### P1-07: Mensagens de erro amigáveis

**Descrição:** Erros da API mostram texto técnico (ex: "400 Bad Request").

**Solução:** Mapear erros comuns para mensagens em português.

**Status:** ✅ Resolvido (Rules.tsx - onError com mapeamento de códigos HTTP)

---

### P1-08: Loading state durante save

**Descrição:** Botão "Criar/Atualizar" não mostra loading.

**Solução:** Usar isPending da mutation para mostrar spinner.

**Status:** ✅ Resolvido (Rules.tsx - Loader2 + saveRule.isPending)

---

### P1-09: Confirmação ao deletar

**Descrição:** Usa confirm() nativo, inconsistente com design.

**Solução:** Usar AlertDialog do shadcn/ui.

**Status:** ✅ Resolvido (Rules.tsx - AlertDialog para delete)

---

### P1-10: Select nativo sem estilização

**Descrição:** Selects usam `<select>` nativo, não componente shadcn.

**Solução:** Usar Select do shadcn/ui para consistência.

**Status:** ❌ Pendente

---

## P2 - POLIMENTO (Consistência / Performance)

### P2-01: RuleFormDialog não utilizado

**Descrição:** Componente existe mas não é usado.

**Solução:** Implementar e usar em Rules.tsx.

**Status:** ❌ Pendente

---

### P2-02: Duplicação de lógica

**Descrição:** Rules.tsx e RulesDidactic.tsx têm código duplicado.

**Solução:** Extrair para RuleFormDialog compartilhado.

**Status:** ❌ Pendente

---

### P2-03: Debounce na validação

**Descrição:** Validação dispara a cada keystroke.

**Solução:** Usar useDebouncedCallback.

**Status:** ❌ Pendente

---

### P2-04: Re-renders desnecessários

**Descrição:** Componente inteiro re-renderiza ao digitar.

**Solução:** Usar React.memo e useCallback.

**Status:** ❌ Pendente

---

### P2-05: Skeleton loading

**Descrição:** Lista mostra spinner genérico.

**Solução:** Usar Skeleton do shadcn/ui.

**Status:** ❌ Pendente

---

### P2-06: Paginação na lista

**Descrição:** Lista carrega todas as regras de uma vez.

**Solução:** Implementar paginação com useInfiniteQuery ou Pagination.

**Status:** ❌ Pendente

---

### P2-07: Ordenação/filtro na lista

**Descrição:** Não há como filtrar ou ordenar regras.

**Solução:** Adicionar filtros por tipo, classificação, status.

**Status:** ❌ Pendente

---

### P2-08: Preview da regra

**Descrição:** Não há preview antes de salvar.

**Solução:** Adicionar seção de preview com formatação legível.

**Status:** ❌ Pendente

---

## PLANO DE IMPLEMENTAÇÃO

### Fase 1: P0 (Críticos) - Estimativa: 2h

1. P0-01: Validação REGEX (30min)
2. P0-02: Validação BETWEEN (30min)
3. P0-03: Validação IN/NOT_IN (30min)
4. P0-04: Unsaved changes warning (20min)
5. P0-05: Conflito de versão (10min)

### Fase 2: P1 (Importantes) - Estimativa: 3h

1. P1-01 a P1-04: Validação e feedback (1h)
2. P1-05 a P1-06: Acessibilidade (45min)
3. P1-07 a P1-10: UX polish (1h15min)

### Fase 3: P2 (Polimento) - Estimativa: 2h

1. P2-01 a P2-02: Refatoração (1h)
2. P2-03 a P2-08: Performance e UX (1h)

### Fase 4: Testes - Estimativa: 1h

1. Unit tests para validações
2. E2E tests para fluxos completos

---

## CRITÉRIO DE CONCLUSÃO

- [x] P0 = 0 pendentes ✅
- [ ] P1 = 0 pendentes (4 pendentes)
- [ ] P2 = 0 pendentes (8 pendentes)
- [x] Testes passando (unit + E2E) ✅
- [x] Build sem erros ✅
- [x] Lint sem erros ✅

---

**Última atualização:** 2024-12-31T13:00:00Z
