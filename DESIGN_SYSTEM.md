# RULEX Design System - Profissional, Tecnológico e Seguro

## 🎨 Filosofia de Design

O RULEX é uma plataforma de **análise e prevenção de fraudes em transações de crédito**. O design deve transmitir:

- **Segurança**: Confiança, proteção, vigilância
- **Tecnologia**: Moderno, inovador, preciso
- **Profissionalismo**: Corporativo, sério, confiável
- **Clareza**: Informações claras, fácil navegação, sem ambiguidades

---

## 🎯 Paleta de Cores

### Cores Primárias

| Cor | Hex | RGB | Uso |
|-----|-----|-----|-----|
| **Azul Segurança** | `#0052CC` | `0, 82, 204` | Botões primários, links, ações principais |
| **Azul Escuro** | `#003D99` | `0, 61, 153` | Hover, foco, ênfase |
| **Azul Claro** | `#E8F0FF` | `232, 240, 255` | Background, highlights |

### Cores de Status

| Status | Cor | Hex | Significado |
|--------|-----|-----|------------|
| **Aprovado** | Verde | `#10B981` | Transação aprovada, sem risco |
| **Suspeito** | Amarelo | `#F59E0B` | Atenção necessária, revisar |
| **Fraude** | Vermelho | `#EF4444` | Bloqueado, alto risco |
| **Neutro** | Cinza | `#6B7280` | Informação, status indefinido |

### Cores Neutras

| Uso | Hex | RGB |
|-----|-----|-----|
| **Branco** | `#FFFFFF` | `255, 255, 255` |
| **Cinza 50** | `#F9FAFB` | `249, 250, 251` |
| **Cinza 100** | `#F3F4F6` | `243, 244, 246` |
| **Cinza 200** | `#E5E7EB` | `229, 231, 235` |
| **Cinza 300** | `#D1D5DB` | `209, 213, 219` |
| **Cinza 400** | `#9CA3AF` | `156, 163, 175` |
| **Cinza 500** | `#6B7280` | `107, 114, 128` |
| **Cinza 600** | `#4B5563` | `75, 85, 99` |
| **Cinza 700** | `#374151` | `55, 65, 81` |
| **Cinza 800** | `#1F2937` | `31, 41, 55` |
| **Cinza 900** | `#111827` | `17, 24, 39` |

---

## 🔤 Tipografia

### Fonte Principal
**Inter** (Google Fonts)
- Moderna, limpa, excelente legibilidade
- Suporta múltiplos pesos: 300, 400, 500, 600, 700, 800

### Hierarquia Tipográfica

| Elemento | Tamanho | Peso | Line Height | Uso |
|----------|---------|------|-------------|-----|
| **H1** | 32px | 700 | 1.2 | Títulos de página |
| **H2** | 24px | 600 | 1.3 | Títulos de seção |
| **H3** | 20px | 600 | 1.4 | Subtítulos |
| **Body Large** | 16px | 400 | 1.5 | Texto principal |
| **Body Regular** | 14px | 400 | 1.5 | Texto padrão |
| **Body Small** | 12px | 400 | 1.4 | Texto secundário |
| **Caption** | 11px | 500 | 1.4 | Labels, hints |
| **Code** | 13px | 500 | 1.5 | Código, IDs |

---

## 🧩 Componentes Principais

### Botões

#### Botão Primário (Azul)
```
Cor: #0052CC
Hover: #003D99
Active: #002D7A
Padding: 12px 24px
Border Radius: 6px
Font: 14px, 600
Sombra: 0 2px 4px rgba(0, 82, 204, 0.2)
```

#### Botão Secundário (Cinza)
```
Cor: #E5E7EB
Texto: #374151
Hover: #D1D5DB
Padding: 12px 24px
Border Radius: 6px
Font: 14px, 600
```

#### Botão Perigo (Vermelho)
```
Cor: #EF4444
Hover: #DC2626
Active: #B91C1C
Padding: 12px 24px
Border Radius: 6px
Font: 14px, 600
Sombra: 0 2px 4px rgba(239, 68, 68, 0.2)
```

#### Botão Sucesso (Verde)
```
Cor: #10B981
Hover: #059669
Active: #047857
Padding: 12px 24px
Border Radius: 6px
Font: 14px, 600
Sombra: 0 2px 4px rgba(16, 185, 129, 0.2)
```

### Cards

```
Background: #FFFFFF
Border: 1px solid #E5E7EB
Border Radius: 8px
Padding: 20px
Sombra: 0 1px 3px rgba(0, 0, 0, 0.1)
Hover Sombra: 0 4px 12px rgba(0, 0, 0, 0.15)
Transition: 0.3s ease
```

### Inputs

```
Background: #FFFFFF
Border: 1px solid #D1D5DB
Border Radius: 6px
Padding: 10px 12px
Font: 14px, 400
Focus: Border #0052CC, Sombra 0 0 0 3px rgba(0, 82, 204, 0.1)
Error: Border #EF4444
Success: Border #10B981
```

### Badges (Status)

#### Aprovado
```
Background: #ECFDF5
Cor: #065F46
Border: 1px solid #A7F3D0
Padding: 4px 12px
Border Radius: 12px
Font: 12px, 600
```

#### Suspeito
```
Background: #FFFBEB
Cor: #92400E
Border: 1px solid #FCD34D
Padding: 4px 12px
Border Radius: 12px
Font: 12px, 600
```

#### Fraude
```
Background: #FEF2F2
Cor: #7F1D1D
Border: 1px solid #FECACA
Padding: 4px 12px
Border Radius: 12px
Font: 12px, 600
```

---

## 📐 Espaçamento

### Escala de Espaçamento
```
4px   (xs)
8px   (sm)
12px  (md)
16px  (lg)
20px  (xl)
24px  (2xl)
32px  (3xl)
40px  (4xl)
48px  (5xl)
```

### Aplicação
- **Padding Interno**: 16px - 20px
- **Margin Entre Elementos**: 12px - 16px
- **Margin Entre Seções**: 24px - 32px
- **Gap em Grids**: 16px - 20px

---

## 🔒 Segurança Visual

### Indicadores de Segurança

#### Ícone de Cadeado (Seguro)
```
Cor: #10B981
Tamanho: 20px
Posição: Canto superior direito
Tooltip: "Transação segura"
```

#### Ícone de Alerta (Suspeito)
```
Cor: #F59E0B
Tamanho: 20px
Posição: Canto superior direito
Tooltip: "Requer verificação"
```

#### Ícone de Bloqueio (Fraude)
```
Cor: #EF4444
Tamanho: 20px
Posição: Canto superior direito
Tooltip: "Transação bloqueada"
```

### Sombras e Profundidade

```
Sombra 1 (Subtle):   0 1px 2px rgba(0, 0, 0, 0.05)
Sombra 2 (Small):    0 1px 3px rgba(0, 0, 0, 0.1)
Sombra 3 (Medium):   0 4px 6px rgba(0, 0, 0, 0.1)
Sombra 4 (Large):    0 10px 15px rgba(0, 0, 0, 0.1)
Sombra 5 (XL):       0 20px 25px rgba(0, 0, 0, 0.1)
```

---

## ♿ Acessibilidade WCAG 2.1 AA

### Contraste de Cores

| Elemento | Contraste Mínimo | Exemplo |
|----------|-----------------|---------|
| Texto em Botão | 4.5:1 | Branco em Azul #0052CC ✅ |
| Texto em Card | 4.5:1 | Cinza #374151 em Branco ✅ |
| Ícones | 3:1 | Verde #10B981 em Branco ✅ |
| Bordas | 3:1 | Cinza #D1D5DB em Branco ✅ |

### Tamanho de Fonte Mínimo
- **Texto Principal**: 14px (mínimo)
- **Texto Secundário**: 12px (com bom contraste)
- **Código**: 13px

### Foco Visível

```
Outline: 2px solid #0052CC
Outline Offset: 2px
Aplicado a: Botões, Links, Inputs, Cards interativos
```

### Espaçamento de Clique

```
Área Mínima: 44x44px
Aplicado a: Botões, Links, Checkboxes, Radios
```

### Indicadores de Estado

- **Hover**: Mudança de cor + sombra
- **Focus**: Outline azul + sombra
- **Active**: Cor mais escura
- **Disabled**: Opacidade 50% + cursor not-allowed

---

## 🎬 Animações e Transições

### Transições Padrão

```
Duração: 200ms - 300ms
Easing: ease-in-out
Propriedades: background-color, color, border-color, box-shadow
```

### Animações Especiais

#### Carregamento
```
Duração: 1.5s
Easing: linear
Animação: Rotação contínua de ícone
```

#### Sucesso
```
Duração: 400ms
Easing: ease-out
Animação: Fade in + scale (0.9 → 1.0)
```

#### Erro
```
Duração: 300ms
Easing: ease-out
Animação: Shake (vibração leve)
```

---

## 📱 Responsividade

### Breakpoints

| Dispositivo | Largura | Uso |
|-------------|---------|-----|
| **Mobile** | < 640px | Smartphones |
| **Tablet** | 640px - 1024px | Tablets |
| **Desktop** | > 1024px | Computadores |
| **Wide** | > 1280px | Monitores grandes |

### Ajustes por Breakpoint

**Mobile (< 640px)**
- Padding: 12px
- Font: -2px
- Botões: Full width
- Cards: Stack vertical

**Tablet (640px - 1024px)**
- Padding: 16px
- Font: -1px
- Botões: 50% width
- Cards: 2 colunas

**Desktop (> 1024px)**
- Padding: 20px
- Font: Normal
- Botões: Auto width
- Cards: 3+ colunas

---

## 🔍 Ícones

### Conjunto de Ícones
**Lucide React** (24px padrão)

### Ícones Principais

| Ação | Ícone | Cor |
|------|-------|-----|
| Aprovado | CheckCircle | #10B981 |
| Suspeito | AlertCircle | #F59E0B |
| Fraude | XCircle | #EF4444 |
| Detalhes | Info | #0052CC |
| Editar | Edit | #0052CC |
| Deletar | Trash | #EF4444 |
| Buscar | Search | #6B7280 |
| Menu | Menu | #374151 |
| Logout | LogOut | #6B7280 |

---

## 📊 Componentes de Dados

### Tabelas

```
Header Background: #F9FAFB
Header Font: 12px, 600, #374151
Row Background: #FFFFFF
Row Hover: #F3F4F6
Border: 1px solid #E5E7EB
Padding: 12px 16px
```

### Gráficos

```
Cor Primária: #0052CC
Cor Secundária: #10B981
Cor Terciária: #F59E0B
Cor Erro: #EF4444
Fundo: #FFFFFF
Grid: #E5E7EB (20% opacidade)
```

### Badges de Métrica

```
Background: #E8F0FF
Cor: #0052CC
Font: 14px, 600
Padding: 8px 12px
Border Radius: 4px
```

---

## 🌙 Modo Escuro (Futuro)

### Paleta Escura

| Elemento | Cor |
|----------|-----|
| Background | #0F172A |
| Surface | #1E293B |
| Border | #334155 |
| Texto Principal | #F1F5F9 |
| Texto Secundário | #CBD5E1 |
| Azul | #3B82F6 |
| Verde | #10B981 |
| Amarelo | #F59E0B |
| Vermelho | #EF4444 |

---

## ✅ Checklist de Implementação

- [ ] Aplicar paleta de cores em todas as páginas
- [ ] Atualizar tipografia com Inter
- [ ] Implementar componentes de botão (primário, secundário, perigo, sucesso)
- [ ] Criar cards com sombras e hover
- [ ] Implementar inputs com foco visível
- [ ] Adicionar badges de status
- [ ] Implementar acessibilidade WCAG 2.1 AA
- [ ] Testar contraste de cores
- [ ] Adicionar indicadores de segurança
- [ ] Implementar responsividade
- [ ] Adicionar animações suaves
- [ ] Testar em múltiplos navegadores
- [ ] Testar com screen readers
- [ ] Validar com ferramentas de acessibilidade

---

## 📚 Referências

- **WCAG 2.1**: https://www.w3.org/WAI/WCAG21/quickref/
- **Material Design**: https://material.io/design
- **Tailwind CSS**: https://tailwindcss.com
- **Lucide Icons**: https://lucide.dev
- **Inter Font**: https://fonts.google.com/specimen/Inter
