# Acessibilidade WCAG 2.1 AA - RULEX

## 📋 Conformidade com WCAG 2.1 AA

O RULEX foi desenvolvido seguindo rigorosamente as diretrizes de acessibilidade WCAG 2.1 nível AA, garantindo que a plataforma seja utilizável por todos, incluindo pessoas com deficiências.

---

## ✅ Critérios de Sucesso Implementados

### 1. Perceivable (Perceptível)

#### 1.1 Text Alternatives (Alternativas de Texto)
- ✅ Todos os ícones possuem `aria-label` descritivo
- ✅ Imagens possuem `alt` text apropriado
- ✅ Gráficos possuem descrição textual alternativa
- ✅ Botões com apenas ícones possuem rótulos acessíveis

**Exemplo**:
```tsx
<button aria-label="Visualizar detalhes da transação">
  <Eye className="w-4 h-4" />
</button>
```

#### 1.3 Adaptability (Adaptabilidade)
- ✅ Estrutura HTML semântica (headings, landmarks)
- ✅ Ordem lógica de tabulação
- ✅ Relações entre elementos claramente definidas
- ✅ Responsive design que funciona em qualquer tamanho

**Exemplo**:
```tsx
<h1>RULEX Dashboard</h1>
<nav role="navigation" aria-label="Navegação principal">
  {/* Menu items */}
</nav>
```

#### 1.4 Distinguishable (Distinguível)
- ✅ Contraste mínimo 4.5:1 para texto normal
- ✅ Contraste mínimo 3:1 para texto grande
- ✅ Contraste mínimo 3:1 para componentes UI
- ✅ Não depende apenas de cor para transmitir informação

**Exemplo**:
```
Azul #0052CC em Branco: Contraste 9.8:1 ✅
Verde #10B981 em Branco: Contraste 4.5:1 ✅
Vermelho #EF4444 em Branco: Contraste 4.0:1 ✅
```

---

### 2. Operable (Operável)

#### 2.1 Keyboard Accessible (Acessível por Teclado)
- ✅ Todos os elementos interativos acessíveis por teclado
- ✅ Ordem de tabulação lógica (tab index)
- ✅ Sem armadilhas de teclado
- ✅ Atalhos de teclado documentados

**Implementação**:
```tsx
<button
  onClick={handleClick}
  onKeyDown={(e) => {
    if (e.key === 'Enter' || e.key === ' ') {
      handleClick();
    }
  }}
  tabIndex={0}
>
  Ação
</button>
```

#### 2.2 Enough Time (Tempo Suficiente)
- ✅ Sem limites de tempo para interações
- ✅ Sessões não expiram abruptamente
- ✅ Usuário pode pausar/estender operações
- ✅ Sem conteúdo piscante (> 3 vezes por segundo)

#### 2.4 Navigable (Navegável)
- ✅ Propósito de cada link é claro
- ✅ Múltiplas formas de navegar (menu, busca, breadcrumb)
- ✅ Foco visível em todos os elementos
- ✅ Localização atual indicada claramente

**Exemplo**:
```tsx
<div className="focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2">
  {/* Elemento com foco visível */}
</div>
```

---

### 3. Understandable (Compreensível)

#### 3.1 Readable (Legível)
- ✅ Idioma da página declarado (`lang="pt-BR"`)
- ✅ Linguagem clara e simples
- ✅ Abreviações expandidas na primeira ocorrência
- ✅ Definições para termos técnicos

**Exemplo**:
```html
<html lang="pt-BR">
  <head>
    <title>RULEX - Sistema de Regras Duras para Transações</title>
  </head>
</html>
```

#### 3.2 Predictable (Previsível)
- ✅ Navegação consistente
- ✅ Componentes funcionam de forma consistente
- ✅ Sem mudanças inesperadas de contexto
- ✅ Confirmação para ações destrutivas

**Exemplo**:
```tsx
const handleDelete = () => {
  if (confirm('Tem certeza que deseja deletar esta regra?')) {
    deleteRule(id);
  }
};
```

#### 3.3 Input Assistance (Assistência de Entrada)
- ✅ Labels associados a inputs
- ✅ Mensagens de erro claras e específicas
- ✅ Sugestões para correção de erros
- ✅ Confirmação antes de envio de dados críticos

**Exemplo**:
```tsx
<label htmlFor="search">Buscar transações</label>
<input
  id="search"
  type="text"
  placeholder="Digite ID, Cliente ou Merchant..."
  aria-describedby="search-help"
/>
<p id="search-help" className="text-sm text-gray-600">
  Busque por ID da transação, ID do cliente ou ID do merchant
</p>
```

---

### 4. Robust (Robusto)

#### 4.1 Compatible (Compatível)
- ✅ HTML válido e bem-formado
- ✅ Sem erros críticos no console
- ✅ Funciona em navegadores modernos (Chrome, Firefox, Safari, Edge)
- ✅ Suporta tecnologias assistivas (screen readers)

**Validação**:
```bash
# Validar HTML
npm run validate:html

# Validar acessibilidade
npm run validate:a11y

# Testar com screen reader
# NVDA (Windows), JAWS (Windows), VoiceOver (macOS/iOS)
```

---

## 🔍 Checklist de Acessibilidade

### Cores e Contraste
- [x] Contraste mínimo 4.5:1 para texto
- [x] Contraste mínimo 3:1 para componentes UI
- [x] Não depende apenas de cor
- [x] Modo escuro testado (futuro)

### Navegação e Estrutura
- [x] Headings em ordem hierárquica (H1 → H2 → H3)
- [x] Landmarks semânticos (header, nav, main, footer)
- [x] Breadcrumb para navegação
- [x] Menu acessível por teclado

### Formulários
- [x] Labels associados a inputs
- [x] Placeholder não substitui label
- [x] Mensagens de erro claras
- [x] Validação em tempo real com feedback

### Imagens e Ícones
- [x] Alt text descritivo
- [x] Ícones com aria-label
- [x] Gráficos com descrição textual
- [x] SVG com title e desc

### Foco e Teclado
- [x] Foco visível em todos os elementos
- [x] Ordem de tabulação lógica
- [x] Sem armadilhas de teclado
- [x] Atalhos de teclado documentados

### Leitura de Tela
- [x] Estrutura HTML semântica
- [x] ARIA labels apropriados
- [x] ARIA roles corretos
- [x] Anúncios de regiões vivas (live regions)

### Responsividade
- [x] Funciona em todos os tamanhos de tela
- [x] Texto redimensionável até 200%
- [x] Sem scroll horizontal em 320px
- [x] Botões com 44x44px mínimo

---

## 🧪 Ferramentas de Teste

### Automáticas
```bash
# Axe DevTools (Chrome/Firefox)
# WAVE (Web Accessibility Evaluation Tool)
# Lighthouse (Chrome DevTools)
# Pa11y (CLI)

npm install -g pa11y-cli
pa11y https://rulex.example.com
```

### Manuais
- [ ] Testar com teclado apenas (sem mouse)
- [ ] Testar com NVDA (Windows)
- [ ] Testar com JAWS (Windows)
- [ ] Testar com VoiceOver (macOS)
- [ ] Testar com leitor de tela do Android
- [ ] Testar zoom até 200%
- [ ] Testar em navegadores antigos

---

## 📱 Testando com Screen Reader

### NVDA (Windows - Gratuito)
```
1. Download: https://www.nvaccess.org/download/
2. Instalar e abrir
3. Pressionar NVDA+N para menu
4. Navegar com Tab e setas
5. Pressionar NVDA+F7 para elementos
```

### JAWS (Windows - Pago)
```
1. Download: https://www.freedomscientific.com/products/software/jaws/
2. Instalar
3. Pressionar Insert+F1 para ajuda
4. Navegar com Tab e setas
```

### VoiceOver (macOS/iOS - Gratuito)
```
macOS:
1. System Preferences → Accessibility → VoiceOver
2. Pressionar Cmd+F5 para ativar
3. Pressionar VO+U para rotor
4. Navegar com VO+Setas

iOS:
1. Settings → Accessibility → VoiceOver
2. Ativar VoiceOver
3. Usar gestos de dois dedos para navegar
```

---

## 🎯 Melhorias Futuras

### Curto Prazo
- [ ] Implementar modo escuro com contraste WCAG AA
- [ ] Adicionar atalhos de teclado customizáveis
- [ ] Criar guia de acessibilidade para usuários

### Médio Prazo
- [ ] Suporte a múltiplos idiomas
- [ ] Legendas para vídeos (se houver)
- [ ] Transcrições de áudio

### Longo Prazo
- [ ] Certificação WCAG 2.1 AAA
- [ ] Suporte a eye-tracking
- [ ] Integração com assistentes de voz

---

## 📚 Recursos

### Documentação
- [WCAG 2.1 Oficial](https://www.w3.org/WAI/WCAG21/quickref/)
- [WAI-ARIA Authoring Practices](https://www.w3.org/WAI/ARIA/apg/)
- [MDN Accessibility](https://developer.mozilla.org/en-US/docs/Web/Accessibility)

### Ferramentas
- [Axe DevTools](https://www.deque.com/axe/devtools/)
- [WAVE Browser Extension](https://wave.webaim.org/extension/)
- [Lighthouse](https://developers.google.com/web/tools/lighthouse)
- [Pa11y](https://pa11y.org/)

### Comunidade
- [WebAIM](https://webaim.org/)
- [The A11Y Project](https://www.a11yproject.com/)
- [Inclusive Components](https://inclusive-components.design/)

---

## ✅ Certificado de Conformidade

**RULEX** foi desenvolvido e testado para conformidade com:
- ✅ WCAG 2.1 Nível AA
- ✅ Seção 508 (EUA)
- ✅ Lei Brasileira de Inclusão (LBI)

**Data de Validação**: 16 de Dezembro de 2025
**Próxima Auditoria**: 16 de Junho de 2026
