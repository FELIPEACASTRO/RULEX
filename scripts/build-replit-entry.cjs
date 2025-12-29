/* eslint-disable no-console */
/* Generated helper: create dist/index.cjs for Replit autoscale */

const fs = require("node:fs");
const path = require("node:path");

const repoRoot = path.resolve(__dirname, "..");
const distDir = path.join(repoRoot, "dist");
const outFile = path.join(distDir, "index.cjs");

fs.mkdirSync(distDir, { recursive: true });

const serverSource = `/**
 * Minimal static file server for Replit.
 *
 * Replit deployments may try to run: "node ./dist/index.cjs".
 * Our Vite build outputs static assets to "./dist/public", so we serve that folder.
 */

const http = require("node:http");
const fs = require("node:fs");
const path = require("node:path");

const port = Number(process.env.PORT || 3000);
const publicDir = path.join(__dirname, "public");

function contentTypeFor(filePath) {
  const ext = path.extname(filePath).toLowerCase();
  switch (ext) {
    case ".html": return "text/html; charset=utf-8";
    case ".css": return "text/css; charset=utf-8";
    case ".js": return "text/javascript; charset=utf-8";
    case ".map": return "application/json; charset=utf-8";
    case ".json": return "application/json; charset=utf-8";
    case ".svg": return "image/svg+xml";
    case ".png": return "image/png";
    case ".jpg":
    case ".jpeg": return "image/jpeg";
    case ".gif": return "image/gif";
    case ".webp": return "image/webp";
    case ".ico": return "image/x-icon";
    case ".txt": return "text/plain; charset=utf-8";
    default: return "application/octet-stream";
  }
}

function safeResolvePublicPath(urlPath) {
  const decoded = decodeURIComponent(urlPath.split("?")[0] || "/");
  const stripped = decoded.replace(/^\\/+/, "");
  const resolved = path.resolve(publicDir, stripped);
  if (!resolved.startsWith(publicDir)) return null;
  return resolved;
}

function send(res, status, body, headers = {}) {
  res.writeHead(status, { "cache-control": "no-store", ...headers });
  res.end(body);
}

const server = http.createServer((req, res) => {
  try {
    if (!req.url) return send(res, 400, "Bad Request");

    // Serve static files (SPA fallback to index.html).
    let filePath = safeResolvePublicPath(req.url);
    if (!filePath) return send(res, 400, "Bad Request");

    const stat = fs.existsSync(filePath) ? fs.statSync(filePath) : null;
    if (stat && stat.isDirectory()) filePath = path.join(filePath, "index.html");

    if (!fs.existsSync(filePath)) {
      // SPA fallback (routes like /login, /rules, etc.)
      filePath = path.join(publicDir, "index.html");
    }

    const stream = fs.createReadStream(filePath);
    stream.on("error", () => send(res, 500, "Internal Server Error"));
    res.writeHead(200, { "content-type": contentTypeFor(filePath), "cache-control": "no-store" });
    stream.pipe(res);
  } catch (err) {
    send(res, 500, "Internal Server Error");
  }
});

server.listen(port, "0.0.0.0", () => {
  console.log(\`Serving \${publicDir} on http://0.0.0.0:\${port}\`);
});
`;

fs.writeFileSync(outFile, serverSource, "utf8");
console.log(`Wrote ${path.relative(repoRoot, outFile)}`);

