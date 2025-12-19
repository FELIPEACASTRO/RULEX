import { useCallback, useMemo, useState } from "react";

type User = {
  id: string;
  name: string;
  email: string;
} | null;

type UseAuthOptions = {
  redirectOnUnauthenticated?: boolean;
  redirectPath?: string;
};

export function useAuth(_options?: UseAuthOptions) {
  const [loading] = useState(false);

  const user: User = useMemo(() => {
    return {
      id: 'dev-user',
      name: 'Usuário Dev',
      email: 'dev@rulex.local'
    };
  }, []);

  const logout = useCallback(async () => {
    console.log('Logout - Java backend handles auth');
  }, []);

  const state = useMemo(() => {
    return {
      user,
      loading,
      error: null,
      isAuthenticated: true,
    };
  }, [user, loading]);

  return {
    ...state,
    refresh: () => Promise.resolve(),
    logout,
  };
}
