## CI/CD Setup

### Overview
- Workflows: `ci.yml`, `deploy-preview.yml`, `deploy-production.yml`
- Gates: type-check, lint, unit tests (coverage), E2E tests, build, audit
- Test pipelines: `premerge` (PRs), `test:ci` (production), `test:smoke` (production validation)

### Test Pipeline Strategy

**PR Pipeline (`premerge`):**
- Runs unit tests (`vitest run`)
- Runs integration tests excluding production-only tests (`playwright test --grep-invert @prod`)
- Fast feedback for developers (typically <2 minutes)
- Excludes HTTPS/Service Worker tests that require production environment

**Production Pipeline (`test:ci`):**
- Full test suite including database setup
- Unit tests with coverage
- All integration tests including production smoke tests
- Used for `predeploy` and production deployments

**Production Validation (`test:smoke`):**
- Dedicated production smoke tests
- HTTPS enforcement, service worker registration
- Performance validation, PWA functionality
- Run after production deployments

### GitHub Actions Workflows
- CI runs on pushes to `main`/`develop` and all PRs.
- Preview deploys on non-`main` pushes using Vercel.
- Production deploy is manual via workflow_dispatch with predeploy checks.

### Required Secrets
- `VERCEL_TOKEN`, `VERCEL_ORG_ID`, `VERCEL_PROJECT_ID`
- `DATABASE_URL`, `NEXTAUTH_SECRET`, `QUICKBASE_TOKEN` (as applicable)

Add secrets under Repository → Settings → Secrets and variables → Actions.

### Branch Protection
- Require all CI checks to pass before merge.
- Require at least one code review.
- Disallow force pushes to `main`.

### Deployment Process
- Preview: automatic on PR; bot comments the preview URL.
- Production: trigger "Deploy Production" workflow; smoke tests run after deployment.

### Troubleshooting
- Inspect failed job logs and downloaded artifacts (coverage, Playwright report).
- Re-run jobs with "Re-run failed jobs".
- Ensure secrets are set and `npm ci` succeeds.

### Monitoring
- Use GitHub checks for status and history.
- Use Vercel dashboard for deployments and quick rollback.


