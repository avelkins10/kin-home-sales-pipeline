# Deployment Runbook - Kin Home Sales Pipeline

**Purpose:** Step-by-step guide for deploying the app to production on Vercel.

---

## ⚠️ CRITICAL: Pre-Deployment Security Fix

**STOP**: Before proceeding with deployment, you MUST complete these security fixes:

### 1. Rotate Exposed Quickbase Token

The Quickbase token was previously exposed via `NEXT_PUBLIC_QUICKBASE_TOKEN` in `.env.local`. This token MUST be rotated immediately:

1. Run the rotation utility: `node scripts/rotate-quickbase-token.js`
2. Follow the on-screen instructions to generate a new token
3. Update `.env.local` with new token (server-side only)
4. Remove any `NEXT_PUBLIC_QUICKBASE_TOKEN` references
5. Test locally: `npm run setup:health`

### 2. Verify Security Fixes

- [ ] No `NEXT_PUBLIC_QUICKBASE_TOKEN` in `.env.local`
- [ ] No `NEXT_PUBLIC_QUICKBASE_TOKEN` in `env.example`
- [ ] New Quickbase token generated and tested
- [ ] `next-sitemap.config.js` exists at project root
- [ ] Test credentials added to `.env.local` (TEST_USER_EMAIL, TEST_USER_PASSWORD)

**Do not proceed with deployment until all items are checked.**

---

## Pre-Deployment Checklist

**Code Readiness:**
- [ ] All tests passing locally (`npm run test:ci`)
- [ ] No TypeScript errors (`npm run type-check`)
- [ ] No ESLint errors (`npm run lint`)
- [ ] Production build succeeds (`npm run build`)
- [ ] Security audit clean (`npm run audit:prod`)
- [ ] All environment variables documented in `env.example`

**Data Readiness:**
- [ ] Production Quickbase token generated and tested
- [ ] Production Neon database created and migrated
- [ ] Production users seeded (real users, not test accounts)
- [ ] Quickbase API permissions verified for production token

**Security Verification:**
- [ ] Verify no `NEXT_PUBLIC_QUICKBASE_TOKEN` in .env.local or Vercel envs
- [ ] Rotate Quickbase token if previously exposed
- [ ] Confirm `next-sitemap.config.js` exists at project root
- [ ] Verify all API routes using `sql` have `export const runtime = 'nodejs'`

**Documentation:**
- [ ] README.md updated with production URLs
- [ ] SETUP.md includes production setup steps
- [ ] SECURITY-READINESS.md reviewed and approved
- [ ] QA-HANDOFF.md signed off

---

## Step 1: Create Vercel Project

**1.1 Sign up for Vercel:**
- Visit https://vercel.com
- Sign up with GitHub account (recommended)
- Verify email address

**1.2 Import GitHub Repository:**
- Click "Add New Project"
- Select GitHub repository: `kin-home-sales-pipeline`
- Click "Import"

**1.3 Configure Project Settings:**
- **Framework Preset:** Next.js (auto-detected)
- **Root Directory:** `./` (default)
- **Build Command:** `npm run build` (default)
- **Output Directory:** `.next` (default)
- **Install Command:** `npm install` (default)
- **Node Version:** 18.x (set in project settings)

**1.4 Configure Environment Variables:**

Click "Environment Variables" and add each variable:

**Quickbase API:**
- `QUICKBASE_REALM` = `kin.quickbase.com` (Production, Preview, Development)
- `QUICKBASE_TOKEN` = `[PRODUCTION_TOKEN]` (Production only, use separate tokens for Preview/Dev)
- `QUICKBASE_APP_ID` = `br9kwm8bk` (All environments)
- `QUICKBASE_TABLE_PROJECTS` = `br9kwm8na` (All environments)
- `QUICKBASE_TABLE_ADDERS` = `bsaycczmf` (All environments)
- `QUICKBASE_TABLE_TASKS` = `br9kwm8q9` (All environments)

**Test Credentials (for smoke tests):**
- `TEST_USER_EMAIL` = `test@kinhome.com` (Production test account email)
- `TEST_USER_PASSWORD` = `[SECURE_PASSWORD]` (Production test account password)

Note: Create a dedicated test user account in the production database with 'closer' role before deployment. This account is used by automated smoke tests to verify login and data access.

**Database:**
- `DATABASE_URL` = `[PRODUCTION_NEON_URL]` (Production only, use separate databases for Preview/Dev)

**NextAuth:**
- `NEXTAUTH_SECRET` = `[GENERATED_SECRET]` (Production only, generate with `openssl rand -base64 32`)
- `NEXTAUTH_URL` = `https://your-app.vercel.app` (Production), `https://your-app-git-[branch].vercel.app` (Preview)

**App Config:**
- `NEXT_PUBLIC_APP_URL` = `https://your-app.vercel.app` (Production)
- `NODE_ENV` = `production` (auto-set by Vercel)

**Sentry (Optional):**
- `SENTRY_DSN` = `[YOUR_SERVER_DSN]` (Production only)
- `NEXT_PUBLIC_SENTRY_DSN` = `[YOUR_CLIENT_DSN]` (Production only)

**Important:** Use different tokens/databases for Production, Preview, and Development environments to prevent test data from affecting production.

**1.5 Deploy:**
- Click "Deploy"
- Wait for build to complete (~2-3 minutes)
- Verify deployment succeeds

---

## Step 2: Post-Deployment Verification

**2.1 Run Production Smoke Tests:**

```bash
TEST_USER_EMAIL=test@kinhome.com TEST_USER_PASSWORD=<password> BASE_URL=https://your-app.vercel.app npm run test:smoke
```

Verify all tests pass:
- ✅ Site is accessible
- ✅ Login flow works
- ✅ Dashboard loads with real data
- ✅ Projects list loads and filters work
- ✅ Project detail page loads
- ✅ PWA manifest is accessible
- ✅ Service worker registers
- ✅ HTTPS is enforced
- ✅ API routes are protected
- ✅ Offline functionality works
- ✅ Performance meets targets

Note: If service worker or HTTPS tests fail, verify BASE_URL starts with https:// and the app is deployed (not running locally).

**2.2 Manual Verification:**

**Test on Desktop (Chrome):**
1. Open `https://your-app.vercel.app`
2. Login with production test account
3. Verify dashboard loads
4. Navigate to projects list
5. Search for a project
6. Click project to view detail
7. Verify timeline displays correctly
8. Test hold management (place/release)
9. Verify offline indicator works (toggle network in DevTools)
10. Check browser console for errors (should be none)

**Test on iPad (Safari):**
1. Open `https://your-app.vercel.app` in Safari
2. Login with production test account
3. Verify responsive layout (1024×768)
4. Test touch interactions (tap, scroll, swipe)
5. Verify traffic lights display correctly
6. Test PWA installation (see Step 3)
7. Verify offline functionality
8. Check for any layout issues

**2.3 Verify Security:**

**Check cookies in browser DevTools:**
- Navigate to Application → Cookies
- Verify session cookie has:
  - Name: `__Secure-next-auth.session-token`
  - Secure: ✓ (checkmark)
  - HttpOnly: ✓ (checkmark)
  - SameSite: Lax
  - Domain: `.vercel.app`

**Check HTTPS:**
- Verify URL starts with `https://`
- Verify no mixed content warnings
- Verify SSL certificate is valid (green padlock)

**Check headers:**
- Open DevTools → Network → Select any request → Headers
- Verify response headers include:
  - `X-Frame-Options: DENY`
  - `X-Content-Type-Options: nosniff`
  - `Referrer-Policy: origin-when-cross-origin`

---

## Step 3: PWA Installation Verification

**3.1 Test on iPad:**

1. Open Safari on iPad
2. Navigate to `https://your-app.vercel.app`
3. Login
4. Tap Share button (square with arrow)
5. Scroll down and tap "Add to Home Screen"
6. Verify app icon preview shows correctly
7. Edit name if desired (default: "Kin Solar")
8. Tap "Add"
9. Verify app icon appears on home screen
10. Tap app icon to launch
11. Verify app opens in standalone mode (no Safari UI)
12. Verify splash screen shows (if configured)
13. Test offline functionality:
    - Enable Airplane Mode
    - Navigate to projects
    - Verify cached data loads
    - Verify offline indicator appears
    - Disable Airplane Mode
    - Verify sync occurs
14. Document findings in `LAUNCH-NOTES.md`

**3.2 Test on Android (Optional):**

1. Open Chrome on Android device
2. Navigate to production URL
3. Tap menu (three dots)
4. Tap "Install app" or "Add to Home Screen"
5. Verify installation prompt shows
6. Tap "Install"
7. Verify app icon appears
8. Launch and test offline functionality

**3.3 Test on Desktop Chrome:**

1. Open Chrome
2. Navigate to production URL
3. Look for install icon in address bar (⊕ or computer icon)
4. Click install icon
5. Click "Install" in dialog
6. Verify app opens in standalone window
7. Test offline functionality

---

## Step 4: Configure Custom Domain (Optional)

**4.1 Add Domain in Vercel:**
- Go to Project Settings → Domains
- Click "Add Domain"
- Enter domain: `pipeline.kinhome.com` (example)
- Click "Add"

**4.2 Configure DNS:**
- Add CNAME record in your DNS provider:
  - Name: `pipeline`
  - Value: `cname.vercel-dns.com`
  - TTL: 3600
- Wait for DNS propagation (5-60 minutes)

**4.3 Update Environment Variables:**
- Update `NEXTAUTH_URL` to `https://pipeline.kinhome.com`
- Update `NEXT_PUBLIC_APP_URL` to `https://pipeline.kinhome.com`
- Redeploy to apply changes

**4.4 Verify SSL Certificate:**
- Vercel automatically provisions SSL certificate
- Verify certificate is valid (green padlock in browser)
- Verify redirects from HTTP to HTTPS work

---

## Step 5: Set Up Monitoring

**5.1 Vercel Analytics (Built-in):**
- Go to Project → Analytics
- Enable Web Analytics (free)
- View metrics: page views, unique visitors, top pages

**5.2 Sentry (Optional):**
- Create Sentry project at https://sentry.io
- Copy DSN
- Add to Vercel environment variables:
  - `SENTRY_DSN` (server)
  - `NEXT_PUBLIC_SENTRY_DSN` (client)
- Redeploy
- Verify errors are captured in Sentry dashboard

**5.3 Vercel Logs:**
- Go to Project → Logs
- Monitor real-time logs during deployment
- Set up log drains for long-term storage (optional)

---

## Step 6: Configure Deployment Triggers

**6.1 Production Deployments:**
- Trigger: Push to `main` branch
- Auto-deploy: Enabled
- Build command: `npm run build`
- Environment: Production

**6.2 Preview Deployments:**
- Trigger: Push to any branch (except `main`)
- Auto-deploy: Enabled
- Environment: Preview
- Unique URL: `https://your-app-git-[branch]-[team].vercel.app`

**6.3 Deployment Protection (Optional):**
- Go to Project Settings → Git
- Enable "Deployment Protection"
- Require approval for production deployments
- Add approvers (team leads, tech leads)

---

## Step 7: Rollback Procedure

**If deployment fails or has critical bugs:**

**7.1 Instant Rollback (Vercel Dashboard):**
1. Go to Project → Deployments
2. Find last known good deployment
3. Click three dots (⋯) → "Promote to Production"
4. Click "Promote"
5. Deployment is live in ~30 seconds

**7.2 Git Rollback:**
1. Identify commit hash of last good deployment
2. Create rollback branch:
   ```bash
   git checkout -b rollback-[issue]
   git revert [bad-commit-hash]
   git push origin rollback-[issue]
   ```
3. Merge to `main` via PR
4. Vercel auto-deploys

**7.3 Environment Variable Rollback:**
1. Go to Project Settings → Environment Variables
2. Click variable to edit
3. View history and select previous value
4. Save
5. Redeploy from Deployments tab

---

## Step 8: Post-Launch Monitoring

**First 24 Hours:**
- [ ] Monitor Vercel logs for errors
- [ ] Check Sentry for error spikes
- [ ] Monitor Quickbase API rate limiting (should stay under 10 req/sec)
- [ ] Verify offline sync is working (check for queued mutations)
- [ ] Monitor user feedback channels
- [ ] Track login success rate
- [ ] Verify no performance degradation

**First Week:**
- [ ] Review analytics (daily active users, page views)
- [ ] Check for any security issues
- [ ] Monitor database performance (Neon dashboard)
- [ ] Verify token hasn't expired
- [ ] Collect user feedback
- [ ] Document any issues in GitHub Issues

**Ongoing:**
- [ ] Weekly review of error logs
- [ ] Monthly security audit (`npm run audit:prod`)
- [ ] Quarterly token rotation (see `TOKEN-ROTATION.md`)
- [ ] Monitor success metrics (see Step 9)

---

## Step 9: Success Metrics

Track these KPIs post-launch (see `docs/SUCCESS-METRICS.md` for details):

**Adoption Metrics:**
- Daily Active Users (target: 90%+ of sales team)
- PWA Installation Rate (target: 80%+ on iPad)
- Login Success Rate (target: >99%)

**Performance Metrics:**
- Dashboard Load Time (target: <2 seconds)
- API Response Time (target: <1 second, 95th percentile)
- Offline Sync Success Rate (target: >95%)

**Business Metrics:**
- Task Completion Rate (target: 30%+ increase vs Quickbase)
- Hold Resolution Time (target: 50% reduction)
- Time to Find Project Status (target: <3 seconds)

---

## Troubleshooting

**Issue: Build fails on Vercel**
- Check build logs in Vercel dashboard
- Verify all dependencies are in `package.json` (not just devDependencies)
- Verify TypeScript compiles locally (`npm run type-check`)
- Check for missing environment variables

**Issue: Environment variables not working**
- Verify variables are set in correct environment (Production/Preview/Development)
- Verify variable names match exactly (case-sensitive)
- Redeploy after changing environment variables
- Check Vercel logs for "undefined" errors

**Issue: Database connection fails**
- Verify `DATABASE_URL` is correct for production Neon database
- Verify Neon database is running (check Neon dashboard)
- Verify database has `pgcrypto` extension enabled
- Verify tables exist (run migration if needed)

**Issue: Quickbase API returns 401**
- Verify `QUICKBASE_TOKEN` is valid (not expired)
- Generate fresh token at https://kin.quickbase.com → My Preferences → My User Token
- Update Vercel environment variable
- Redeploy

**Issue: Service worker not registering**
- Verify app is served over HTTPS (required for service workers)
- Check browser console for service worker errors
- Verify `public/sw.js` is accessible at `/sw.js`
- Clear browser cache and reload

**Issue: PWA not installable**
- Verify `manifest.json` is accessible at `/manifest.json`
- Verify all required manifest fields are present
- Verify icons exist at specified paths
- Check Lighthouse PWA audit for issues

---

## Emergency Contacts

**Technical Issues:**
- Developer On-Call: [PHONE/EMAIL]
- Tech Lead: [PHONE/EMAIL]
- DevOps: [PHONE/EMAIL]

**Business Issues:**
- Product Owner: [PHONE/EMAIL]
- Sales Leadership: [PHONE/EMAIL]

**Vendor Support:**
- Quickbase Support: https://help.quickbase.com
- Vercel Support: https://vercel.com/support
- Neon Support: https://neon.tech/docs/introduction

---

**✅ Deployment Complete - Monitor for 24 hours before announcing to full team.**
