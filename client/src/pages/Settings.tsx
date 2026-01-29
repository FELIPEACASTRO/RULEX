import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Switch } from "@/components/ui/switch";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Settings as SettingsIcon, Bell, Shield, Database, Users, Key, Save, CheckCircle, AlertTriangle, Info } from "lucide-react";
import { useState, useEffect, useCallback } from "react";
import { useLocation } from "wouter";
import { toast } from "sonner";

// Tipo para as configurações persistidas
interface UserSettings {
  general: {
    companyName: string;
    timezone: string;
    maintenanceMode: boolean;
  };
  notifications: {
    emailAlerts: boolean;
    pushNotifications: boolean;
    dailySummary: boolean;
  };
  security: {
    twoFactorAuth: boolean;
    sessionTimeout: boolean;
    sessionTimeoutMinutes: number;
  };
  integrations: {
    apiEndpoint: string;
    apiKey: string;
    webhookActive: boolean;
  };
}

const DEFAULT_SETTINGS: UserSettings = {
  general: {
    companyName: "",
    timezone: "America/Sao_Paulo",
    maintenanceMode: false,
  },
  notifications: {
    emailAlerts: true,
    pushNotifications: true,
    dailySummary: false,
  },
  security: {
    twoFactorAuth: false,
    sessionTimeout: true,
    sessionTimeoutMinutes: 30,
  },
  integrations: {
    apiEndpoint: "",
    apiKey: "",
    webhookActive: false,
  },
};

const STORAGE_KEY = "rulex_user_settings";

function loadSettings(): UserSettings {
  try {
    const stored = localStorage.getItem(STORAGE_KEY);
    if (stored) {
      const parsed = JSON.parse(stored) as UserSettings;
      return {
        ...DEFAULT_SETTINGS,
        ...parsed,
        integrations: {
          ...DEFAULT_SETTINGS.integrations,
          ...parsed.integrations,
          apiKey: "",
        },
      };
    }
  } catch (e) {
    console.error("Erro ao carregar configurações:", e);
  }
  return DEFAULT_SETTINGS;
}

function saveSettings(settings: UserSettings): void {
  try {
    const sanitized: UserSettings = {
      ...settings,
      integrations: {
        ...settings.integrations,
        apiKey: "",
      },
    };
    localStorage.setItem(STORAGE_KEY, JSON.stringify(sanitized));
  } catch (e) {
    console.error("Erro ao salvar configurações:", e);
  }
}

export default function Settings() {
  const [settings, setSettings] = useState<UserSettings>(loadSettings);
  const [savedSection, setSavedSection] = useState<string | null>(null);
  const [, setLocation] = useLocation();

  // Carregar configurações ao montar
  useEffect(() => {
    setSettings(loadSettings());
  }, []);

  // Handler genérico para atualizar configurações
  const updateSetting = useCallback(
    <K extends keyof UserSettings>(
      section: K,
      field: keyof UserSettings[K],
      value: UserSettings[K][keyof UserSettings[K]]
    ) => {
      setSettings((prev) => ({
        ...prev,
        [section]: {
          ...prev[section],
          [field]: value,
        },
      }));
    },
    []
  );

  // Salvar uma seção específica
  const handleSave = useCallback(
    (section: string) => {
      saveSettings(settings);
      setSavedSection(section);
      toast.success(`Configurações de ${section} salvas com sucesso`);
      setTimeout(() => setSavedSection(null), 2000);
    },
    [settings]
  );

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold tracking-tight">Configurações</h1>
        <p className="text-muted-foreground">
          Gerencie as configurações do sistema e preferências
        </p>
      </div>

      {/* Aviso sobre limitações */}
      <div className="bg-blue-50 border border-blue-200 text-blue-800 px-4 py-3 rounded flex items-start gap-2">
        <Info className="h-5 w-5 mt-0.5 flex-shrink-0" />
        <div>
          <p className="font-medium">Nota sobre funcionalidades</p>
          <p className="text-sm">
            As preferências de notificação e configurações gerais são salvas localmente no navegador.
            Configurações de segurança avançadas (2FA, gerenciamento de usuários) requerem implementação no backend.
          </p>
        </div>
      </div>

      <Tabs defaultValue="general" className="space-y-4">
        <TabsList>
          <TabsTrigger value="general" className="flex items-center gap-2">
            <SettingsIcon className="h-4 w-4" />
            Geral
          </TabsTrigger>
          <TabsTrigger value="notifications" className="flex items-center gap-2">
            <Bell className="h-4 w-4" />
            Notificações
          </TabsTrigger>
          <TabsTrigger value="security" className="flex items-center gap-2">
            <Shield className="h-4 w-4" />
            Segurança
          </TabsTrigger>
          <TabsTrigger value="integrations" className="flex items-center gap-2">
            <Database className="h-4 w-4" />
            Integrações
          </TabsTrigger>
        </TabsList>

        <TabsContent value="general" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Configurações Gerais</CardTitle>
              <CardDescription>
                Configure as opções básicas do sistema
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-2">
                <Label htmlFor="company-name">Nome da Empresa</Label>
                <Input
                  id="company-name"
                  placeholder="Sua empresa"
                  value={settings.general.companyName}
                  onChange={(e) => updateSetting("general", "companyName", e.target.value)}
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="timezone">Fuso Horário</Label>
                <Input
                  id="timezone"
                  placeholder="America/Sao_Paulo"
                  value={settings.general.timezone}
                  onChange={(e) => updateSetting("general", "timezone", e.target.value)}
                />
              </div>
              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label>Modo de Manutenção</Label>
                  <p className="text-sm text-muted-foreground">
                    Desativa temporariamente o processamento de transações
                  </p>
                </div>
                <Switch
                  checked={settings.general.maintenanceMode}
                  onCheckedChange={(checked) =>
                    updateSetting("general", "maintenanceMode", checked)
                  }
                />
              </div>
              {settings.general.maintenanceMode && (
                <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded flex items-center gap-2">
                  <AlertTriangle className="h-4 w-4" />
                  <span className="text-sm">
                    Modo de manutenção ativado. O processamento de transações está pausado.
                  </span>
                </div>
              )}
              <Button onClick={() => handleSave("Geral")}>
                {savedSection === "Geral" ? (
                  <>
                    <CheckCircle className="h-4 w-4 mr-2" />
                    Salvo!
                  </>
                ) : (
                  <>
                    <Save className="h-4 w-4 mr-2" />
                    Salvar Alterações
                  </>
                )}
              </Button>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="notifications" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Preferências de Notificação</CardTitle>
              <CardDescription>
                Configure como você deseja receber alertas
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label>Alertas por Email</Label>
                  <p className="text-sm text-muted-foreground">
                    Receba alertas críticos por email
                  </p>
                </div>
                <Switch
                  checked={settings.notifications.emailAlerts}
                  onCheckedChange={(checked) =>
                    updateSetting("notifications", "emailAlerts", checked)
                  }
                />
              </div>
              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label>Notificações Push</Label>
                  <p className="text-sm text-muted-foreground">
                    Receba notificações no navegador
                  </p>
                </div>
                <Switch
                  checked={settings.notifications.pushNotifications}
                  onCheckedChange={(checked) =>
                    updateSetting("notifications", "pushNotifications", checked)
                  }
                />
              </div>
              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label>Resumo Diário</Label>
                  <p className="text-sm text-muted-foreground">
                    Receba um resumo diário das atividades
                  </p>
                </div>
                <Switch
                  checked={settings.notifications.dailySummary}
                  onCheckedChange={(checked) =>
                    updateSetting("notifications", "dailySummary", checked)
                  }
                />
              </div>
              <Button onClick={() => handleSave("Notificações")}>
                {savedSection === "Notificações" ? (
                  <>
                    <CheckCircle className="h-4 w-4 mr-2" />
                    Salvo!
                  </>
                ) : (
                  <>
                    <Save className="h-4 w-4 mr-2" />
                    Salvar Preferências
                  </>
                )}
              </Button>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="security" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Key className="h-5 w-5" />
                Configurações de Segurança
              </CardTitle>
              <CardDescription>
                Gerencie a segurança da sua conta e do sistema
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label>Autenticação de Dois Fatores</Label>
                  <p className="text-sm text-muted-foreground">
                    Adicione uma camada extra de segurança
                  </p>
                </div>
                <Switch
                  checked={settings.security.twoFactorAuth}
                  onCheckedChange={(checked) =>
                    updateSetting("security", "twoFactorAuth", checked)
                  }
                />
              </div>
              {settings.security.twoFactorAuth && (
                <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded flex items-center gap-2">
                  <AlertTriangle className="h-4 w-4" />
                  <span className="text-sm">
                    2FA requer implementação no backend. Esta configuração foi salva localmente.
                  </span>
                </div>
              )}
              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label>Timeout de Sessão</Label>
                  <p className="text-sm text-muted-foreground">
                    Encerrar sessão após inatividade
                  </p>
                </div>
                <Switch
                  checked={settings.security.sessionTimeout}
                  onCheckedChange={(checked) =>
                    updateSetting("security", "sessionTimeout", checked)
                  }
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="session-timeout">Tempo de Inatividade (minutos)</Label>
                <Input
                  id="session-timeout"
                  type="number"
                  placeholder="30"
                  value={settings.security.sessionTimeoutMinutes}
                  onChange={(e) =>
                    updateSetting(
                      "security",
                      "sessionTimeoutMinutes",
                      parseInt(e.target.value, 10) || 30
                    )
                  }
                  disabled={!settings.security.sessionTimeout}
                />
              </div>
              <Button onClick={() => handleSave("Segurança")}>
                {savedSection === "Segurança" ? (
                  <>
                    <CheckCircle className="h-4 w-4 mr-2" />
                    Salvo!
                  </>
                ) : (
                  <>
                    <Save className="h-4 w-4 mr-2" />
                    Atualizar Segurança
                  </>
                )}
              </Button>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Users className="h-5 w-5" />
                Gerenciamento de Usuários
              </CardTitle>
              <CardDescription>
                Gerencie os usuários e permissões do sistema
              </CardDescription>
            </CardHeader>
            <CardContent>
              <Button variant="outline" onClick={() => setLocation("/users")}>
                Gerenciar Usuários
              </Button>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="integrations" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Integrações</CardTitle>
              <CardDescription>
                Configure as integrações com sistemas externos
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-2">
                <Label htmlFor="api-endpoint">Endpoint da API</Label>
                <Input
                  id="api-endpoint"
                  placeholder="https://api.exemplo.com"
                  value={settings.integrations.apiEndpoint}
                  onChange={(e) =>
                    updateSetting("integrations", "apiEndpoint", e.target.value)
                  }
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="api-key">Chave da API</Label>
                <Input
                  id="api-key"
                  type="password"
                  placeholder="••••••••••••"
                  value={settings.integrations.apiKey}
                  onChange={(e) =>
                    updateSetting("integrations", "apiKey", e.target.value)
                  }
                />
                <p className="text-xs text-muted-foreground">
                  A chave não é persistida no navegador por segurança.
                </p>
              </div>
              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label>Webhook Ativo</Label>
                  <p className="text-sm text-muted-foreground">
                    Enviar eventos para webhook externo
                  </p>
                </div>
                <Switch
                  checked={settings.integrations.webhookActive}
                  onCheckedChange={(checked) =>
                    updateSetting("integrations", "webhookActive", checked)
                  }
                />
              </div>
              <Button onClick={() => handleSave("Integrações")}>
                {savedSection === "Integrações" ? (
                  <>
                    <CheckCircle className="h-4 w-4 mr-2" />
                    Salvo!
                  </>
                ) : (
                  <>
                    <Save className="h-4 w-4 mr-2" />
                    Salvar Integrações
                  </>
                )}
              </Button>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}
