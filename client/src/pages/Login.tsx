import { useAuth } from "@/_core/hooks/useAuth";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { useState } from "react";
import { useLocation } from "wouter";

export default function Login() {
  const { loginWithBasicAuth } = useAuth();
  const [, setLocation] = useLocation();
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState<string | null>(null);

  return (
    <div className="min-h-screen flex items-center justify-center bg-background p-4">
      <Card className="w-full max-w-md">
        <CardHeader>
          <CardTitle>Login</CardTitle>
          <CardDescription>
            Ambiente local/homolog: autenticação via <strong>HTTP Basic</strong>.
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          {error ? (
            <Alert variant="destructive">
              <AlertDescription>{error}</AlertDescription>
            </Alert>
          ) : null}

          <div className="space-y-2">
            <Label htmlFor="username">Usuário</Label>
            <Input
              id="username"
              value={username}
              onChange={e => setUsername(e.target.value)}
              placeholder="admin"
              autoComplete="username"
            />
          </div>

          <div className="space-y-2">
            <Label htmlFor="password">Senha</Label>
            <Input
              id="password"
              type="password"
              value={password}
              onChange={e => setPassword(e.target.value)}
              placeholder="rulex"
              autoComplete="current-password"
            />
          </div>

          <Button
            className="w-full"
            onClick={async () => {
              try {
                setError(null);
                if (!username.trim() || !password.trim()) {
                  setError("Informe usuário e senha.");
                  return;
                }
                await loginWithBasicAuth(username.trim(), password);
                setLocation("/");
              } catch (err) {
                setError((err as Error).message || "Falha no login.");
              }
            }}
          >
            Entrar
          </Button>
        </CardContent>
      </Card>
    </div>
  );
}

