import { useCallback, useEffect, useMemo, useState } from "react";
import {
  clearBasicAuth,
  clearTokens,
  getAccessToken,
  getBasicAuthRaw,
  getRefreshToken,
  setBasicAuthRaw,
  setTokens,
} from "@/_core/auth/tokens";
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
const BASIC_AUTH_RAW_FROM_ENV = import.meta.env.VITE_API_BASIC_AUTH as string | undefined;
const ENABLE_BEARER_AUTH = import.meta.env.VITE_ENABLE_BEARER_AUTH === "true";
const AUTH_BASE_URL = JAVA_API_BASE_URL.replace(/\/$/, "").endsWith("/api")
  ? JAVA_API_BASE_URL.replace(/\/$/, "")
  : `${JAVA_API_BASE_URL.replace(/\/$/, "")}/api`;

function basicAuthToUser(raw: string): User {
  const username = raw.split(":")[0]?.trim() || "user";
  return {
    id: `basic:${username}`,
    name: username,
    email: username.includes("@") ? username : `${username}@local`,
  };
}

async function fetchMe(token: string, signal?: AbortSignal): Promise<User> {
  const response = await fetch(`${AUTH_BASE_URL}/auth/me`, {
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

  const response = await fetch(`${AUTH_BASE_URL}/auth/refresh`, {
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
      const token = ENABLE_BEARER_AUTH ? getAccessToken() : null;
      const basicAuthRaw = BASIC_AUTH_RAW_FROM_ENV || getBasicAuthRaw() || null;

      // Dev/HML: se houver Basic Auth configurado, consideramos "logado" sem depender
      // de endpoints /api/auth/* (que podem não existir no backend).
      if (!token && basicAuthRaw) {
        setUser(basicAuthToUser(basicAuthRaw));
        setError(null);
        setLoading(false);
        return;
      }

      if (!ENABLE_BEARER_AUTH) {
        setLoading(false);
        if (redirectOnUnauthenticated) {
          window.location.href = getLoginUrl();
        }
        return;
      }

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
          clearBasicAuth();
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
    if (!ENABLE_BEARER_AUTH) {
      clearTokens();
      clearBasicAuth();
      setUser(null);
      setError(null);
      setLoading(false);
      window.location.href = getLoginUrl();
      return;
    }
    try {
      await fetch(`${AUTH_BASE_URL}/auth/logout`, {
        method: "POST",
        headers: { "Content-Type": "application/json", Accept: "application/json" },
      });
    } catch (err) {
      console.warn("Logout falhou", err);
    } finally {
      clearTokens();
      clearBasicAuth();
      setUser(null);
      setError(null);
      setLoading(false);
      window.location.href = getLoginUrl();
    }
  }, []);

  const loginWithToken = useCallback(async (accessToken: string, refreshToken?: string) => {
    if (!ENABLE_BEARER_AUTH) {
      throw new Error("Bearer auth disabled in this environment");
    }
    setTokens(accessToken, refreshToken);
    const me = await fetchMe(accessToken);
    setUser(me);
    setError(null);
    return me;
  }, []);

  const loginWithBasicAuth = useCallback(async (username: string, password: string) => {
    setBasicAuthRaw(username, password);
    const raw = `${username}:${password}`;
    const me = basicAuthToUser(raw);
    setUser(me);
    setError(null);
    setLoading(false);
    return me;
  }, []);

  const refresh = useCallback(async () => {
    if (!ENABLE_BEARER_AUTH) {
      throw new Error("Bearer auth disabled in this environment");
    }
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
    loginWithBasicAuth,
    logout,
  };
}
