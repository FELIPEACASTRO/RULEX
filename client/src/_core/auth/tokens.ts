const ACCESS_TOKEN_KEY = "rulex_access_token";
const REFRESH_TOKEN_KEY = "rulex_refresh_token";
const BASIC_AUTH_KEY = "rulex_basic_auth";

export function getAccessToken() {
  return localStorage.getItem(ACCESS_TOKEN_KEY) || null;
}

export function getRefreshToken() {
  return localStorage.getItem(REFRESH_TOKEN_KEY) || null;
}

export function setTokens(accessToken: string | null, refreshToken?: string | null) {
  if (accessToken) {
    localStorage.setItem(ACCESS_TOKEN_KEY, accessToken);
  } else {
    localStorage.removeItem(ACCESS_TOKEN_KEY);
  }

  if (refreshToken !== undefined) {
    if (refreshToken) {
      localStorage.setItem(REFRESH_TOKEN_KEY, refreshToken);
    } else {
      localStorage.removeItem(REFRESH_TOKEN_KEY);
    }
  }
}

export function clearTokens() {
  localStorage.removeItem(ACCESS_TOKEN_KEY);
  localStorage.removeItem(REFRESH_TOKEN_KEY);
}

/**
 * Basic Auth (dev/hml)
 * Armazena no formato "usuario:senha" (NUNCA em produção real).
 */
export function getBasicAuthRaw() {
  return localStorage.getItem(BASIC_AUTH_KEY) || null;
}

export function setBasicAuthRaw(username: string, password: string) {
  localStorage.setItem(BASIC_AUTH_KEY, `${username}:${password}`);
}

export function clearBasicAuth() {
  localStorage.removeItem(BASIC_AUTH_KEY);
}
