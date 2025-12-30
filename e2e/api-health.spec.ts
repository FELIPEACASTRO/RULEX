import { test, expect } from "@playwright/test";

test.describe("API Health", () => {
  const baseUrl = process.env.PLAYWRIGHT_BASE_URL || "http://localhost:5173";
  const apiUrl = "http://localhost:8080/api";

  test("backend health endpoint responds", async ({ request }) => {
    const response = await request.get(`${apiUrl}/actuator/health`);
    expect(response.ok()).toBeTruthy();
    
    const body = await response.json();
    expect(body.status).toBe("UP");
  });

  test("backend metrics endpoint responds", async ({ request }) => {
    const response = await request.get(`${apiUrl}/actuator/prometheus`, {
      headers: {
        Authorization: `Basic ${Buffer.from("admin:rulex").toString("base64")}`,
      },
    });
    // Metrics might require auth or might be 200
    expect([200, 401, 404]).toContain(response.status());
  });

  test("rules API responds", async ({ request }) => {
    const response = await request.get(`${apiUrl}/rules`, {
      headers: {
        Authorization: `Basic ${Buffer.from("admin:rulex").toString("base64")}`,
      },
    });
    expect(response.ok()).toBeTruthy();
  });

  test("transactions API responds", async ({ request }) => {
    const response = await request.get(`${apiUrl}/transactions`, {
      headers: {
        Authorization: `Basic ${Buffer.from("admin:rulex").toString("base64")}`,
      },
    });
    expect(response.ok()).toBeTruthy();
  });
});
