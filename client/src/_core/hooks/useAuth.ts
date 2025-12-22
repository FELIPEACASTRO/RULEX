import { useCallback, useEffect, useMemo, useState } from "react";
import { clearTokens, getAccessToken, getRefreshToken, setTokens } from "@/_core/auth/tokens";
import { getLoginUrl } from "@/const";

type User = {
  id: string;
  name: string;
  email: string;
} | null;

type UseAuthOptions = {
  redirectOnUnauthenticated?: boolean;
  redirectPath?: string;
};

const JAVA_API_BASE_URL = import.meta.env.VITE_JAVA_API_URL || "http://localhost:8080";

async function fetchMe(token: string, signal?: AbortSignal): Promise<User> {
  const response = await fetch(`${JAVA_API_BASE_URL}/api/auth/me`, {
    headers: {
      Accept: "application/json",
      Authorization: `Bearer ${token}`,
    },
    signal,
  });

  if (!response.ok) {
    throw new Error(`Falha ao carregar usuário (${response.status})`);
  }

  const data = await response.json();
  return data ?? null;
}

async function refreshTokens(): Promise<{ accessToken: string; refreshToken?: string }> {
  const refreshToken = getRefreshToken();
  if (!refreshToken) {
    throw new Error("Refresh token ausente");
  }

  const response = await fetch(`${JAVA_API_BASE_URL}/api/auth/refresh`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Accept: "application/json",
    },
    body: JSON.stringify({ refreshToken }),
  });

  if (!response.ok) {
    throw new Error("Falha no refresh token");
  }

  const data = await response.json();
  if (!data.accessToken) {
    throw new Error("Resposta inválida do refresh");
  }

  setTokens(data.accessToken, data.refreshToken ?? refreshToken);
  return { accessToken: data.accessToken, refreshToken: data.refreshToken ?? refreshToken };
}

export function useAuth(options?: UseAuthOptions) {
  const redirectOnUnauthenticated = options?.redirectOnUnauthenticated ?? false;
  const [loading, setLoading] = useState(true);
  const [user, setUser] = useState<User>(null);
  const [error, setError] = useState<Error | null>(null);

  useEffect(() => {
    let active = true;
    const controller = new AbortController();

    const loadUser = async () => {
      const token = getAccessToken();
      if (!token) {
        setLoading(false);
        if (redirectOnUnauthenticated) {
          window.location.href = getLoginUrl();
        }
        return;
      }

      try {
        const currentUser = await fetchMe(token, controller.signal);
        if (!active) return;
        setUser(currentUser);
        setError(null);
      } catch (err) {
        if (!active) return;
        try {
          const refreshed = await refreshTokens();
          const currentUser = await fetchMe(refreshed.accessToken, controller.signal);
          if (!active) return;
          setUser(currentUser);
          setError(null);
        } catch (refreshErr) {
          if (!active) return;
          clearTokens();
          setUser(null);
          setError(refreshErr as Error);
          if (redirectOnUnauthenticated) {
            window.location.href = getLoginUrl();
          }
        }
      } finally {
        if (active) {
          setLoading(false);
        }
      }
    };

    loadUser();

    return () => {
      active = false;
      controller.abort();
    };
  }, [redirectOnUnauthenticated]);

  const logout = useCallback(async () => {
    try {
      await fetch(`${JAVA_API_BASE_URL}/api/auth/logout`, {
        method: "POST",
        headers: { "Content-Type": "application/json", Accept: "application/json" },
      });
    } catch (err) {
      console.warn("Logout falhou", err);
    } finally {
      clearTokens();
      setUser(null);
      setError(null);
      setLoading(false);
      window.location.href = getLoginUrl();
    }
  }, []);

  const loginWithToken = useCallback(async (accessToken: string, refreshToken?: string) => {
    setTokens(accessToken, refreshToken);
    const me = await fetchMe(accessToken);
    setUser(me);
    setError(null);
    return me;
  }, []);

  const refresh = useCallback(async () => {
    const refreshed = await refreshTokens();
    const me = await fetchMe(refreshed.accessToken);
    setUser(me);
    setError(null);
    return me;
  }, []);

  const state = useMemo(() => {
    return {
      user,
      loading,
      error,
      isAuthenticated: !!user,
    };
  }, [user, loading, error]);

  return {
    ...state,
    refresh,
    loginWithToken,
    logout,
  };
}
