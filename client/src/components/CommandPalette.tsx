/**
 * CommandPalette - Paleta de comandos para navegação rápida
 *
 * Atalho: Ctrl+K / Cmd+K
 * Permite busca rápida e navegação por todas as funcionalidades do sistema.
 *
 * @version 1.0.0
 */

import { useEffect, useState } from 'react';
import { useLocation } from 'wouter';
import {
  CommandDialog,
  CommandEmpty,
  CommandGroup,
  CommandInput,
  CommandItem,
  CommandList,
  CommandSeparator,
} from '@/components/ui/command';
import {
  LayoutDashboard,
  Users,
  Zap,
  Layers,
  FileText,
  Settings,
  Activity,
  Shield,
  Moon,
  Sun,
  Search,
} from 'lucide-react';
import { useTheme } from '@/contexts/ThemeContext';

interface Command {
  id: string;
  label: string;
  description?: string;
  icon: React.ComponentType<{ className?: string }>;
  action: () => void;
  category: string;
  keywords?: string[];
}

export function CommandPalette() {
  const [open, setOpen] = useState(false);
  const [, setLocation] = useLocation();
  const { theme, toggleTheme, switchable } = useTheme();

  // Atalho Ctrl+K / Cmd+K
  useEffect(() => {
    const down = (e: KeyboardEvent) => {
      if (e.key === 'k' && (e.metaKey || e.ctrlKey)) {
        e.preventDefault();
        setOpen((open) => !open);
      }
    };

    document.addEventListener('keydown', down);
    return () => document.removeEventListener('keydown', down);
  }, []);

  const commands: Command[] = [
    // Navegação
    {
      id: 'nav-dashboard',
      label: 'Dashboard',
      description: 'Visão geral do sistema',
      icon: LayoutDashboard,
      action: () => {
        setLocation('/');
        setOpen(false);
      },
      category: 'Navegação',
      keywords: ['home', 'início', 'painel'],
    },
    {
      id: 'nav-transactions',
      label: 'Transações',
      description: 'Visualizar transações',
      icon: Users,
      action: () => {
        setLocation('/transactions');
        setOpen(false);
      },
      category: 'Navegação',
      keywords: ['transactions', 'pagamentos'],
    },
    {
      id: 'nav-simulator',
      label: 'Simulador',
      description: 'Testar regras de fraude',
      icon: Zap,
      action: () => {
        setLocation('/simulator');
        setOpen(false);
      },
      category: 'Navegação',
      keywords: ['test', 'teste', 'simular'],
    },
    {
      id: 'nav-rules',
      label: 'Regras de Fraude',
      description: 'Gerenciar regras',
      icon: Layers,
      action: () => {
        setLocation('/rules');
        setOpen(false);
      },
      category: 'Navegação',
      keywords: ['rules', 'fraud', 'fraude'],
    },
    {
      id: 'nav-audit',
      label: 'Auditoria',
      description: 'Logs e histórico',
      icon: FileText,
      action: () => {
        setLocation('/audit');
        setOpen(false);
      },
      category: 'Navegação',
      keywords: ['logs', 'histórico', 'audit'],
    },
    {
      id: 'nav-monitoring',
      label: 'Monitoramento',
      description: 'Status do sistema',
      icon: Activity,
      action: () => {
        setLocation('/monitoring');
        setOpen(false);
      },
      category: 'Navegação',
      keywords: ['status', 'health', 'metrics'],
    },
    // Ações
    ...(switchable
      ? [
          {
            id: 'action-theme',
            label: theme === 'light' ? 'Ativar Dark Mode' : 'Ativar Light Mode',
            description: 'Alternar tema',
            icon: theme === 'light' ? Moon : Sun,
            action: () => {
              toggleTheme?.();
              setOpen(false);
            },
            category: 'Ações',
            keywords: ['theme', 'dark', 'light', 'tema'],
          },
        ]
      : []),
  ];

  const groupedCommands = commands.reduce((acc, cmd) => {
    if (!acc[cmd.category]) {
      acc[cmd.category] = [];
    }
    acc[cmd.category].push(cmd);
    return acc;
  }, {} as Record<string, Command[]>);

  return (
    <CommandDialog open={open} onOpenChange={setOpen}>
      <CommandInput placeholder="Digite um comando ou busque..." />
      <CommandList>
        <CommandEmpty>Nenhum resultado encontrado.</CommandEmpty>
        {Object.entries(groupedCommands).map(([category, cmds], index) => (
          <div key={category}>
            {index > 0 && <CommandSeparator />}
            <CommandGroup heading={category}>
              {cmds.map((cmd) => (
                <CommandItem
                  key={cmd.id}
                  onSelect={cmd.action}
                  className="cursor-pointer"
                >
                  <cmd.icon className="mr-2 h-4 w-4" />
                  <div className="flex flex-col">
                    <span>{cmd.label}</span>
                    {cmd.description && (
                      <span className="text-xs text-muted-foreground">
                        {cmd.description}
                      </span>
                    )}
                  </div>
                </CommandItem>
              ))}
            </CommandGroup>
          </div>
        ))}
      </CommandList>
    </CommandDialog>
  );
}
