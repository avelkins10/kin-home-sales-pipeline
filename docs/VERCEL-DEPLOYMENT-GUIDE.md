# Vercel Deployment Guide - Quick Reference

## Prerequisites
- [ ] Quickbase production token generated and tested
- [ ] Neon production database created and migrated
- [ ] Production test user account created (for smoke tests)
- [ ] All security fixes applied (no exposed tokens)
- [ ] Local build succeeds: `npm run build`

## Step 1: Create Vercel Project

1. Go to https://vercel.com/new
2. Import GitHub repository
3. Configure framework: Next.js (auto-detected)
4. Do NOT deploy yet - click "Environment Variables" first

## Step 2: Configure Environment Variables

**Production Environment** (click "Production" tab):

**Quickbase API:**
- `QUICKBASE_REALM` = `kin.quickbase.com`
- `QUICKBASE_TOKEN` = `[PRODUCTION_TOKEN]`
- `QUICKBASE_APP_ID` = `br9kwm8bk`
- `QUICKBASE_TABLE_PROJECTS` = `br9kwm8na`
- `QUICKBASE_TABLE_ADDERS` = `bsaycczmf`
- `QUICKBASE_TABLE_TASKS` = `br9kwm8q9`

**Database:**
- `DATABASE_URL` = `[PRODUCTION_NEON_URL]`

**NextAuth:**
- `NEXTAUTH_SECRET` = `[GENERATED_SECRET]`
- `NEXTAUTH_URL` = `https://your-app.vercel.app`

**App Config:**
- `NEXT_PUBLIC_APP_URL` = `https://your-app.vercel.app`
- `NODE_ENV` = `production`

**Test Credentials:**
- `TEST_USER_EMAIL` = `test@kinhome.com`
- `TEST_USER_PASSWORD` = `[SECURE_PASSWORD]`

**Sentry (Optional):**
- `SENTRY_DSN` = `[YOUR_SERVER_DSN]`
- `NEXT_PUBLIC_SENTRY_DSN` = `[YOUR_CLIENT_DSN]`

**Preview Environment** (click "Preview" tab):

Use same variables as Production but with:
- `QUICKBASE_TOKEN` = `[PREVIEW_TOKEN]` (separate token)
- `DATABASE_URL` = `[PREVIEW_NEON_URL]` (separate database)
- `NEXTAUTH_URL` = `https://your-app-git-[branch].vercel.app`
- `NEXT_PUBLIC_APP_URL` = `https://your-app-git-[branch].vercel.app`

**Development Environment** (optional):

Use same variables as Preview but with:
- `QUICKBASE_TOKEN` = `[DEV_TOKEN]` (separate token)
- `DATABASE_URL` = `[DEV_NEON_URL]` (separate database)
- `NEXTAUTH_URL` = `http://localhost:3000`
- `NEXT_PUBLIC_APP_URL` = `http://localhost:3000`

## Step 3: Deploy

1. Click "Deploy" button
2. Wait 2-3 minutes for build
3. Verify deployment succeeds (green checkmark)
4. Copy production URL: `https://your-app-xxxxx.vercel.app`

## Step 4: Run Smoke Tests

```bash
TEST_USER_EMAIL=test@kinhome.com \
TEST_USER_PASSWORD=<password> \
BASE_URL=https://your-app-xxxxx.vercel.app \
npm run test:smoke
```

Expect: 11 tests pass (or 9 if @prod tests skipped locally)

## Step 5: Manual Verification

1. Open production URL in Chrome
2. Login with test account
3. Verify dashboard loads with real data
4. Navigate to projects list
5. Click a project to view detail
6. Test hold management (place/release hold)
7. Check browser console for errors (should be none)

## Step 6: Monitor

1. Go to Vercel dashboard → Logs
2. Monitor for 30 minutes
3. Check for error spikes or failed requests
4. Verify Quickbase API calls succeed

## Rollback Procedure

If deployment fails:
1. Go to Vercel dashboard → Deployments
2. Find last known good deployment
3. Click ⋯ → "Promote to Production"
4. Deployment rolls back in ~30 seconds

## Troubleshooting

**Build fails with "siteUrl is required"**
- Verify `next-sitemap.config.js` exists
- Check `NEXT_PUBLIC_APP_URL` is set in Vercel

**Smoke tests fail with 401 Unauthorized**
- Verify test user exists in production database
- Check `TEST_USER_EMAIL` and `TEST_USER_PASSWORD` are correct

**Service worker tests fail**
- Verify BASE_URL starts with https://
- Skip @prod tests if running locally: `playwright test --grep-invert @prod`

**Database connection fails**
- Verify `DATABASE_URL` is correct for production Neon database
- Check Neon dashboard shows database is running
- Verify pgcrypto extension is enabled
