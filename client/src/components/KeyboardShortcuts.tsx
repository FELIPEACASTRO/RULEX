/**
 * KeyboardShortcuts - Painel de atalhos de teclado
 *
 * Exibe todos os atalhos disponíveis no sistema.
 * Atalho para abrir: Shift+? ou ?
 *
 * @version 1.0.0
 */

import { useEffect, useState } from 'react';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog';
import { Badge } from '@/components/ui/badge';
import { Kbd } from '@/components/ui/kbd';
import { Keyboard } from 'lucide-react';

interface Shortcut {
  keys: string[];
  description: string;
  category: string;
}

const shortcuts: Shortcut[] = [
  // Navegação
  {
    keys: ['Ctrl', 'K'],
    description: 'Abrir paleta de comandos',
    category: 'Navegação',
  },
  {
    keys: ['?'],
    description: 'Exibir atalhos de teclado',
    category: 'Navegação',
  },
  {
    keys: ['Esc'],
    description: 'Fechar diálogos e modais',
    category: 'Navegação',
  },
  // Regras
  {
    keys: ['Ctrl', 'S'],
    description: 'Salvar regra atual',
    category: 'Regras',
  },
  {
    keys: ['Ctrl', 'N'],
    description: 'Nova regra',
    category: 'Regras',
  },
  {
    keys: ['Ctrl', 'Shift', 'R'],
    description: 'Resetar formulário',
    category: 'Regras',
  },
  // Simulador
  {
    keys: ['Ctrl', 'Enter'],
    description: 'Executar simulação',
    category: 'Simulador',
  },
  // Tema
  {
    keys: ['Ctrl', 'Shift', 'T'],
    description: 'Alternar Dark/Light Mode',
    category: 'Aparência',
  },
];

export function KeyboardShortcuts() {
  const [open, setOpen] = useState(false);

  useEffect(() => {
    const down = (e: KeyboardEvent) => {
      // Shift+? ou apenas ?
      if (e.key === '?' && !e.ctrlKey && !e.metaKey) {
        e.preventDefault();
        setOpen((open) => !open);
      }
    };

    document.addEventListener('keydown', down);
    return () => document.removeEventListener('keydown', down);
  }, []);

  const groupedShortcuts = shortcuts.reduce((acc, shortcut) => {
    if (!acc[shortcut.category]) {
      acc[shortcut.category] = [];
    }
    acc[shortcut.category].push(shortcut);
    return acc;
  }, {} as Record<string, Shortcut[]>);

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogContent className="max-w-2xl max-h-[80vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            <Keyboard className="h-5 w-5" />
            Atalhos de Teclado
          </DialogTitle>
          <DialogDescription>
            Atalhos disponíveis para aumentar sua produtividade
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-6 mt-4">
          {Object.entries(groupedShortcuts).map(([category, items]) => (
            <div key={category}>
              <h3 className="font-semibold mb-3 text-sm text-muted-foreground uppercase tracking-wide">
                {category}
              </h3>
              <div className="space-y-2">
                {items.map((shortcut, index) => (
                  <div
                    key={index}
                    className="flex items-center justify-between py-2 px-3 rounded-lg hover:bg-muted/50 transition-colors"
                  >
                    <span className="text-sm">{shortcut.description}</span>
                    <div className="flex items-center gap-1">
                      {shortcut.keys.map((key, i) => (
                        <span key={i} className="flex items-center gap-1">
                          <Kbd>{key}</Kbd>
                          {i < shortcut.keys.length - 1 && (
                            <span className="text-muted-foreground">+</span>
                          )}
                        </span>
                      ))}
                    </div>
                  </div>
                ))}
              </div>
            </div>
          ))}
        </div>

        <div className="mt-6 pt-4 border-t">
          <p className="text-xs text-muted-foreground text-center">
            Pressione <Kbd>?</Kbd> a qualquer momento para ver esta lista
          </p>
        </div>
      </DialogContent>
    </Dialog>
  );
}
