import { useEffect, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { Plus, Edit2, Trash2, ToggleRight } from 'lucide-react';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog';

interface Rule {
  id: number;
  ruleName: string;
  description: string;
  ruleType: string;
  threshold: number;
  weight: number;
  enabled: boolean;
  classification: string;
  version: number;
}

/**
 * Página de configuração dinâmica de regras.
 */
export default function Rules() {
  const [rules, setRules] = useState<Rule[]>([]);
  const [loading, setLoading] = useState(true);
  const [editingRule, setEditingRule] = useState<Rule | null>(null);
  const [showDialog, setShowDialog] = useState(false);
  const [formData, setFormData] = useState({
    ruleName: '',
    description: '',
    ruleType: 'SECURITY',
    threshold: 0,
    weight: 0,
    enabled: true,
    classification: 'SUSPICIOUS',
  });

  useEffect(() => {
    fetchRules();
  }, []);

  const fetchRules = async () => {
    setLoading(true);
    try {
      const response = await fetch('/api/rules?page=0&size=100');
      const data = await response.json();
      setRules(data.content || []);
    } catch (error) {
      console.error('Erro ao buscar regras:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleEdit = (rule: Rule) => {
    setEditingRule(rule);
    setFormData({
      ruleName: rule.ruleName,
      description: rule.description,
      ruleType: rule.ruleType,
      threshold: rule.threshold,
      weight: rule.weight,
      enabled: rule.enabled,
      classification: rule.classification,
    });
    setShowDialog(true);
  };

  const handleSave = async () => {
    try {
      const url = editingRule ? `/api/rules/${editingRule.id}` : '/api/rules';
      const method = editingRule ? 'PUT' : 'POST';

      const response = await fetch(url, {
        method,
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(formData),
      });

      if (response.ok) {
        fetchRules();
        setShowDialog(false);
        setEditingRule(null);
        setFormData({
          ruleName: '',
          description: '',
          ruleType: 'SECURITY',
          threshold: 0,
          weight: 0,
          enabled: true,
          classification: 'SUSPICIOUS',
        });
      }
    } catch (error) {
      console.error('Erro ao salvar regra:', error);
    }
  };

  const handleDelete = async (id: number) => {
    if (!confirm('Tem certeza que deseja deletar esta regra?')) return;

    try {
      const response = await fetch(`/api/rules/${id}`, { method: 'DELETE' });
      if (response.ok) {
        fetchRules();
      }
    } catch (error) {
      console.error('Erro ao deletar regra:', error);
    }
  };

  const handleToggle = async (id: number) => {
    try {
      const response = await fetch(`/api/rules/${id}/toggle`, { method: 'PATCH' });
      if (response.ok) {
        fetchRules();
      }
    } catch (error) {
      console.error('Erro ao alternar regra:', error);
    }
  };

  const getRuleTypeColor = (type: string) => {
    switch (type) {
      case 'SECURITY':
        return 'bg-blue-100 text-blue-800';
      case 'CONTEXT':
        return 'bg-purple-100 text-purple-800';
      case 'VELOCITY':
        return 'bg-orange-100 text-orange-800';
      case 'ANOMALY':
        return 'bg-pink-100 text-pink-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const getClassificationColor = (classification: string) => {
    switch (classification) {
      case 'APPROVED':
        return 'bg-green-100 text-green-800';
      case 'SUSPICIOUS':
        return 'bg-amber-100 text-amber-800';
      case 'FRAUD':
        return 'bg-red-100 text-red-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-foreground">Configuração de Regras</h1>
          <p className="text-muted-foreground mt-1">Gerenciar regras de detecção de fraude</p>
        </div>
        <Dialog open={showDialog} onOpenChange={setShowDialog}>
          <DialogTrigger asChild>
            <Button
              onClick={() => {
                setEditingRule(null);
                setFormData({
                  ruleName: '',
                  description: '',
                  ruleType: 'SECURITY',
                  threshold: 0,
                  weight: 0,
                  enabled: true,
                  classification: 'SUSPICIOUS',
                });
              }}
            >
              <Plus className="h-4 w-4 mr-2" />
              Nova Regra
            </Button>
          </DialogTrigger>
          <DialogContent className="max-w-2xl">
            <DialogHeader>
              <DialogTitle>{editingRule ? 'Editar Regra' : 'Nova Regra'}</DialogTitle>
              <DialogDescription>
                Configure os parâmetros da regra de detecção de fraude
              </DialogDescription>
            </DialogHeader>
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-foreground mb-2">Nome da Regra</label>
                <Input
                  value={formData.ruleName}
                  onChange={(e) => setFormData({ ...formData, ruleName: e.target.value })}
                  placeholder="Ex: LOW_AUTHENTICATION_SCORE"
                  disabled={!!editingRule}
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-foreground mb-2">Descrição</label>
                <Input
                  value={formData.description}
                  onChange={(e) => setFormData({ ...formData, description: e.target.value })}
                  placeholder="Descrição da regra"
                />
              </div>
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Tipo de Regra</label>
                  <select
                    value={formData.ruleType}
                    onChange={(e) => setFormData({ ...formData, ruleType: e.target.value })}
                    className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                  >
                    <option value="SECURITY">Segurança</option>
                    <option value="CONTEXT">Contexto</option>
                    <option value="VELOCITY">Velocidade</option>
                    <option value="ANOMALY">Anomalia</option>
                  </select>
                </div>
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Classificação</label>
                  <select
                    value={formData.classification}
                    onChange={(e) => setFormData({ ...formData, classification: e.target.value })}
                    className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                  >
                    <option value="APPROVED">Aprovada</option>
                    <option value="SUSPICIOUS">Suspeita</option>
                    <option value="FRAUD">Fraude</option>
                  </select>
                </div>
              </div>
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Threshold</label>
                  <Input
                    type="number"
                    value={formData.threshold}
                    onChange={(e) => setFormData({ ...formData, threshold: parseInt(e.target.value) })}
                    placeholder="0"
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Peso (0-100)</label>
                  <Input
                    type="number"
                    min="0"
                    max="100"
                    value={formData.weight}
                    onChange={(e) => setFormData({ ...formData, weight: parseInt(e.target.value) })}
                    placeholder="0"
                  />
                </div>
              </div>
              <div className="flex items-center gap-2">
                <input
                  type="checkbox"
                  id="enabled"
                  checked={formData.enabled}
                  onChange={(e) => setFormData({ ...formData, enabled: e.target.checked })}
                  className="w-4 h-4 rounded border-input"
                />
                <label htmlFor="enabled" className="text-sm font-medium text-foreground">
                  Habilitada
                </label>
              </div>
              <div className="flex justify-end gap-2 pt-4">
                <Button variant="outline" onClick={() => setShowDialog(false)}>
                  Cancelar
                </Button>
                <Button onClick={handleSave}>
                  {editingRule ? 'Atualizar' : 'Criar'}
                </Button>
              </div>
            </div>
          </DialogContent>
        </Dialog>
      </div>

      {/* Tabela de Regras */}
      <Card>
        <CardHeader>
          <CardTitle>Regras Configuradas</CardTitle>
          <CardDescription>Total: {rules.length} regras</CardDescription>
        </CardHeader>
        <CardContent>
          {loading ? (
            <div className="flex items-center justify-center h-64">
              <div className="text-center">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-2"></div>
                <p className="text-muted-foreground">Carregando regras...</p>
              </div>
            </div>
          ) : rules.length === 0 ? (
            <div className="flex items-center justify-center h-64">
              <p className="text-muted-foreground">Nenhuma regra configurada</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="border-b border-border">
                  <tr>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Nome da Regra</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Tipo</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Threshold</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Peso</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Classificação</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Status</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Ações</th>
                  </tr>
                </thead>
                <tbody>
                  {rules.map((rule) => (
                    <tr key={rule.id} className="border-b border-border hover:bg-muted/50 transition-colors">
                      <td className="py-3 px-4 text-sm font-medium text-foreground">{rule.ruleName}</td>
                      <td className="py-3 px-4 text-sm">
                        <Badge className={getRuleTypeColor(rule.ruleType)}>
                          {rule.ruleType}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-sm text-center text-foreground">{rule.threshold}</td>
                      <td className="py-3 px-4 text-sm text-center text-foreground">{rule.weight}%</td>
                      <td className="py-3 px-4 text-sm">
                        <Badge className={getClassificationColor(rule.classification)}>
                          {rule.classification}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-sm text-center">
                        <Badge variant={rule.enabled ? 'default' : 'secondary'}>
                          {rule.enabled ? 'Ativa' : 'Inativa'}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-center space-x-2">
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => handleToggle(rule.id)}
                          title={rule.enabled ? 'Desativar' : 'Ativar'}
                        >
                          <ToggleRight className="h-4 w-4" />
                        </Button>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => handleEdit(rule)}
                          title="Editar"
                        >
                          <Edit2 className="h-4 w-4" />
                        </Button>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => handleDelete(rule.id)}
                          title="Deletar"
                          className="text-red-600 hover:text-red-700"
                        >
                          <Trash2 className="h-4 w-4" />
                        </Button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
