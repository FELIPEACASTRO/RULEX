const BASIC_AUTH_KEY = "rulex_basic_auth";

export function getBasicAuthRaw(): string | null {
  try {
    return localStorage.getItem(BASIC_AUTH_KEY) || null;
  } catch {
    return null;
  }
}

export function setBasicAuthRaw(raw: string | null) {
  try {
    if (raw) {
      localStorage.setItem(BASIC_AUTH_KEY, raw);
    } else {
      localStorage.removeItem(BASIC_AUTH_KEY);
    }
  } catch {
    // no-op
  }
}

export function clearBasicAuth() {
  setBasicAuthRaw(null);
}
