# Manual Quadruple Check (evidence)

This folder contains versionable evidence artifacts for the `/manual` pipeline and consistency checks.

## What was run
- `pnpm manual:generate`
- `pnpm manual:check` (=`pnpm manual:generate && pnpm test && pnpm build`)

## Evidence files
- `manual-generate.log.txt` — full output from `pnpm manual:generate`
- `manual-check.log.txt` — full output from `pnpm manual:check`
- `git-branch.txt` / `git-commit.txt` — git context during capture
- `git-status-porcelain.txt` / `git-diff-stat.txt` — working tree state during capture

## Notes
- `manual:generate` will **fail** (non-zero exit) if FE vs BE operator sets diverge.
- The AstValidator function allowlist currently produces non-blocking warnings when the backend extractor cannot find the referenced functions.
