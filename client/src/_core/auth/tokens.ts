/**
 * Token Management for RULEX Authentication
 * 
 * ⚠️ SEC-002 WARNING: Este módulo usa localStorage para armazenar tokens.
 * 
 * RISCOS DE SEGURANÇA:
 * - localStorage é acessível via JavaScript, vulnerável a XSS
 * - Credenciais Basic Auth em texto plano podem ser extraídas por scripts maliciosos
 * 
 * RECOMENDAÇÕES PARA PRODUÇÃO:
 * 1. Migrar para cookies httpOnly com flag Secure
 * 2. Implementar JWT com refresh tokens
 * 3. Usar session storage para tokens de curta duração
 * 4. Implementar CSP headers para mitigar XSS
 * 
 * Este código é aceitável APENAS para ambientes de desenvolvimento/staging.
 * Para produção, implemente autenticação baseada em cookies httpOnly.
 */

const ACCESS_TOKEN_KEY = "rulex_access_token";
const REFRESH_TOKEN_KEY = "rulex_refresh_token";
const BASIC_AUTH_KEY = "rulex_basic_auth";

// SEC-002: Flag para detectar ambiente de produção
const isProduction = import.meta.env.PROD && !import.meta.env.VITE_ALLOW_INSECURE_AUTH;

/**
 * Emite aviso de segurança em produção
 */
function warnIfProduction(operation: string): void {
  if (isProduction) {
    console.warn(
      `⚠️ SEC-002: ${operation} em localStorage não é seguro em produção. ` +
      `Considere migrar para cookies httpOnly.`
    );
  }
}

export function getAccessToken(): string | null {
  return localStorage.getItem(ACCESS_TOKEN_KEY) || null;
}

export function getRefreshToken(): string | null {
  return localStorage.getItem(REFRESH_TOKEN_KEY) || null;
}

export function setTokens(accessToken: string | null, refreshToken?: string | null): void {
  warnIfProduction("Armazenamento de tokens");
  
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

export function clearTokens(): void {
  localStorage.removeItem(ACCESS_TOKEN_KEY);
  localStorage.removeItem(REFRESH_TOKEN_KEY);
}

/**
 * Basic Auth (dev/hml)
 * 
 * ⚠️ SEC-002 CRITICAL: Armazena credenciais em texto plano no localStorage.
 * NUNCA use em produção real - vulnerável a XSS.
 * 
 * Para produção, implemente:
 * - Cookies httpOnly com flag Secure
 * - Autenticação via OAuth2/OIDC
 * - Session-based authentication
 */
export function getBasicAuthRaw(): string | null {
  return localStorage.getItem(BASIC_AUTH_KEY) || null;
}

/**
 * ⚠️ SEC-002 CRITICAL: Armazena username:password em texto plano.
 * Apenas para desenvolvimento/staging.
 */
export function setBasicAuthRaw(username: string, password: string): void {
  warnIfProduction("Armazenamento de credenciais Basic Auth");
  
  // SEC-002: Validação básica para evitar credenciais vazias
  if (!username || !password) {
    console.error("SEC-002: Tentativa de armazenar credenciais vazias");
    return;
  }
  
  localStorage.setItem(BASIC_AUTH_KEY, `${username}:${password}`);
}

export function clearBasicAuth(): void {
  localStorage.removeItem(BASIC_AUTH_KEY);
}

/**
 * Limpa todas as credenciais armazenadas (logout completo)
 */
export function clearAllAuth(): void {
  clearTokens();
  clearBasicAuth();
}
