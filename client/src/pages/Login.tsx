import { useAuth } from "@/_core/hooks/useAuth";
import { getLoginUrl } from "@/const";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { Button } from "@/components/ui/button";
import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { useEffect, useMemo, useState } from "react";
import { useLocation } from "wouter";

export default function Login() {
  const [, setLocation] = useLocation();
  const { user, loading, loginWithBasicAuth } = useAuth();

  const [username, setUsername] = useState("admin");
  const [password, setPassword] = useState("adminpw");
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const loginUrl = useMemo(() => getLoginUrl(), []);
  const isExternalLogin = /^https?:\/\//i.test(loginUrl);

  useEffect(() => {
    if (!loading && user) {
      setLocation("/");
    }
  }, [loading, user, setLocation]);

  const handleExternalLogin = () => {
    window.location.href = loginUrl;
  };

  const handleBasicLogin = async () => {
    setError(null);
    const trimmedUsername = username.trim();
    const trimmedPassword = password;

    if (!trimmedUsername || !trimmedPassword) {
      setError("Informe usuário e senha.");
      return;
    }

    try {
      setSubmitting(true);
      await loginWithBasicAuth(trimmedUsername, trimmedPassword);
      setLocation("/");
    } catch (e) {
      setError(e instanceof Error ? e.message : "Falha ao autenticar");
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center bg-background px-4">
      <Card className="w-full max-w-xl">
        <CardHeader>
          <CardTitle>Login</CardTitle>
          <CardDescription>
            {isExternalLogin
              ? "Continue para o provedor de autenticação."
              : "Ambiente local: use seu usuário/senha do backend (HTTP Basic)."}
          </CardDescription>
        </CardHeader>

        <CardContent className="space-y-4">
          {error && (
            <Alert variant="destructive">
              <AlertTitle>Não foi possível autenticar</AlertTitle>
              <AlertDescription>{error}</AlertDescription>
            </Alert>
          )}

          {isExternalLogin ? (
            <Button className="w-full" onClick={handleExternalLogin} disabled={submitting}>
              Ir para o login
            </Button>
          ) : (
            <>
              <div className="space-y-2">
                <div className="text-sm font-medium">Usuário</div>
                <Input value={username} onChange={(e) => setUsername(e.target.value)} />
              </div>

              <div className="space-y-2">
                <div className="text-sm font-medium">Senha</div>
                <Input
                  type="password"
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                />
              </div>

              <Button className="w-full" onClick={handleBasicLogin} disabled={submitting}>
                Entrar
              </Button>
            </>
          )}
        </CardContent>

        <CardFooter className="justify-center">
          <Button variant="ghost" onClick={() => setLocation("/")}>Voltar</Button>
        </CardFooter>
      </Card>
    </div>
  );
}
