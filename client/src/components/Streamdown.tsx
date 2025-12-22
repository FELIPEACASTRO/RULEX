import React from "react";

type StreamdownProps = {
  children: string;
  className?: string;
};

/**
 * Minimal fallback renderer for markdown-like content.
 *
 * This exists to avoid a hard dependency on an external markdown renderer.
 * It renders content as plain text while preserving whitespace.
 */
export function Streamdown({ children, className }: StreamdownProps) {
  return (
    <pre className={className} style={{ whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
      {children}
    </pre>
  );
}
