# .env.local Setup Instructions

## ⚠️ IMPORTANT: Manual Setup Required

The `.env.local` file cannot be created automatically because it's gitignored for security. Follow these steps to set it up correctly.

---

## Quick Setup (3 minutes)

### Step 1: Create the file
```bash
cd /Users/austinelkins/Rep_Dashboard
cp env.example .env.local
```

### Step 2: Add Arrivy credentials

Open `.env.local` in your editor and find the Arrivy section (around line 105). Add these variables:

```bash
# =============================================================================
# ARRIVY CONFIGURATION
# =============================================================================

# Arrivy API Authentication
ARRIVY_AUTH_KEY=0a27a7e3-e6b5
ARRIVY_AUTH_TOKEN=5730gWxBjDzbQDEeFh3zrs

# Your company name as it appears in Arrivy
# Get this from: https://app.arrivy.com/ → Settings → Company Profile
ARRIVY_COMPANY_NAME=your_company_name_here

# Generate webhook secret:
# Run: openssl rand -base64 32
# Then paste the output here:
ARRIVY_WEBHOOK_SECRET=your_generated_webhook_secret_here

# These can stay as default:
ARRIVY_BASE_URL=https://app.arrivy.com/api
ARRIVY_RATE_LIMIT=30
```

### Step 3: Generate webhook secret
```bash
openssl rand -base64 32
```

Copy the output and paste it as the value for `ARRIVY_WEBHOOK_SECRET` in `.env.local`.

Example output:
```
x8K3mN9pL2qR5tY7vZ1aC4bD6eF8gH0jK2lM4nP6rS8=
```

Your `.env.local` should then have:
```bash
ARRIVY_WEBHOOK_SECRET=x8K3mN9pL2qR5tY7vZ1aC4bD6eF8gH0jK2lM4nP6rS8=
```

### Step 4: Fix DATABASE_URL

**CRITICAL:** Ensure `DATABASE_URL` is a **single line** without any newline characters.

```bash
# ❌ WRONG (with newline):
DATABASE_URL=postgresql://user:pass@host:5432/db
?sslmode=require

# ✅ CORRECT (single line):
DATABASE_URL=postgresql://user:pass@host:5432/db?sslmode=require
```

### Step 5: Find your Arrivy company name

1. Log into https://app.arrivy.com/
2. Go to **Settings** → **Company Profile**
3. Look for **"Company Name"** or **"URL Slug"**
4. Copy the exact name (case-sensitive)
5. Paste it as `ARRIVY_COMPANY_NAME` in `.env.local`

Example:
```bash
ARRIVY_COMPANY_NAME=kin-home
```

---

## Verification Checklist

After setup, verify everything is correct:

```bash
# 1. File exists
ls -la .env.local
# Expected: File should be present

# 2. File is gitignored
git status .env.local
# Expected: "fatal: pathspec '.env.local' did not match any files"
# (This means it's properly ignored)

# 3. All Arrivy variables are set
grep "ARRIVY_" .env.local
# Expected: 6 lines with values (not placeholders)

# 4. DATABASE_URL is single line
grep "DATABASE_URL" .env.local | wc -l
# Expected: 1 (if you see 2+, you have newlines to fix)

# 5. No placeholder values remain
grep "your_.*_here" .env.local
# Expected: Should only show placeholders for unrelated variables (Twilio, OpenAI, etc.)
# Your Arrivy variables should have actual values
```

---

## Complete Arrivy Section Example

Here's what your Arrivy section should look like when complete:

```bash
# =============================================================================
# ARRIVY CONFIGURATION
# =============================================================================
# Arrivy API Authentication (obtain from Arrivy dashboard)
# Get your credentials at: https://app.arrivy.com/settings/integrations
ARRIVY_AUTH_KEY=0a27a7e3-e6b5
ARRIVY_AUTH_TOKEN=5730gWxBjDzbQDEeFh3zrs

# Arrivy Company Name (for customer tracker URLs)
# This is your company name as shown in tracker URLs
# Example: https://app.arrivy.com/live/track/{company_name}/{url_safe_id}
ARRIVY_COMPANY_NAME=kin-home

# Arrivy Webhook Secret (for webhook signature verification)
# Set this to a secure random string and configure the same in Arrivy webhook settings
# Generate with: openssl rand -base64 32
ARRIVY_WEBHOOK_SECRET=x8K3mN9pL2qR5tY7vZ1aC4bD6eF8gH0jK2lM4nP6rS8=

# Arrivy API Base URL (default: https://app.arrivy.com/api)
# Only change this if you're using a custom Arrivy instance
ARRIVY_BASE_URL=https://app.arrivy.com/api

# Arrivy Rate Limit (requests per second)
# Arrivy API allows up to 30 requests per minute = 0.5 requests per second
# Default: 30 (requests per minute)
ARRIVY_RATE_LIMIT=30
```

---

## Testing the Configuration

After setup, test that everything works:

```bash
# 1. Start development server
npm run dev

# 2. Check logs for successful initialization
# Expected output should include:
# "[Arrivy] Client initialized successfully"

# 3. Test webhook endpoint
curl http://localhost:3000/api/webhooks/arrivy

# Expected response:
# {"status":"ok","service":"arrivy-webhook"}
```

---

## Troubleshooting

### Error: "Arrivy not configured"

**Cause:** Environment variables not loaded

**Fix:**
```bash
# Verify .env.local exists in project root
ls -la .env.local

# Verify variables are set correctly
grep "ARRIVY_" .env.local

# Restart development server
npm run dev
```

### Error: "Database connection failed"

**Cause:** DATABASE_URL has newline characters

**Fix:**
```bash
# Check for newlines
cat -A .env.local | grep DATABASE_URL
# Look for $ symbols (indicates line breaks)

# Fix: Edit .env.local and make DATABASE_URL a single line
# Before:
# DATABASE_URL=postgresql://user:pass@host:5432/db
# ?sslmode=require

# After:
# DATABASE_URL=postgresql://user:pass@host:5432/db?sslmode=require
```

### Error: "Webhook signature verification failed"

**Cause:** Webhook secret doesn't match between .env.local and Arrivy dashboard

**Fix:**
```bash
# 1. Check your secret
grep ARRIVY_WEBHOOK_SECRET .env.local

# 2. Copy the exact value
# 3. Go to Arrivy dashboard → Settings → Webhooks
# 4. Paste the exact same secret in webhook configuration
# 5. Save both files
```

### Error: "Tracker URL shows 404"

**Cause:** Company name doesn't match Arrivy

**Fix:**
```bash
# 1. Check your company name
grep ARRIVY_COMPANY_NAME .env.local

# 2. Log into Arrivy and verify exact company name
#    Settings → Company Profile → Company Name
# 3. Update .env.local with exact match (case-sensitive)
# 4. Restart server
```

---

## Security Best Practices

### ✅ DO:
- Keep `.env.local` in `.gitignore`
- Use different secrets for dev/staging/production
- Rotate secrets periodically (every 90 days)
- Store production secrets in Vercel environment variables
- Use strong webhook secrets (32+ characters)

### ❌ DON'T:
- Commit `.env.local` to version control
- Share secrets via email or Slack
- Use the same secrets across environments
- Store secrets in code comments
- Copy/paste secrets into public forums

---

## Production Deployment

When deploying to production, set these as environment variables in Vercel:

```bash
# Using Vercel CLI:
vercel env add ARRIVY_AUTH_KEY production
# Enter: 0a27a7e3-e6b5

vercel env add ARRIVY_AUTH_TOKEN production
# Enter: 5730gWxBjDzbQDEeFh3zrs

vercel env add ARRIVY_COMPANY_NAME production
# Enter: your_company_name

vercel env add ARRIVY_WEBHOOK_SECRET production
# Enter: (generate a NEW secret for production)

vercel env add ARRIVY_BASE_URL production
# Enter: https://app.arrivy.com/api

vercel env add ARRIVY_RATE_LIMIT production
# Enter: 30
```

**Important:** Generate a **different** webhook secret for production!

```bash
# Generate production webhook secret
openssl rand -base64 32

# Add to Vercel
vercel env add ARRIVY_WEBHOOK_SECRET production
# Paste the new secret

# Update Arrivy webhook with new production secret
# Go to: https://app.arrivy.com/settings/integrations/webhooks
# Set webhook URL to: https://your-production-domain.com/api/webhooks/arrivy
# Set secret to the new production secret
```

---

## Next Steps

After completing `.env.local` setup:

1. ✅ Run database migration (see `ARRIVY_DEPLOYMENT_GUIDE.md` Phase 2)
2. ✅ Test locally (`npm run dev`)
3. ✅ Configure Arrivy webhook (see `ARRIVY_DEPLOYMENT_GUIDE.md` Phase 4)
4. ✅ Deploy to production (see `ARRIVY_DEPLOYMENT_GUIDE.md` Phase 5)

---

## Quick Reference

```bash
# Create .env.local
cp env.example .env.local

# Generate webhook secret
openssl rand -base64 32

# Required Arrivy variables:
ARRIVY_AUTH_KEY=0a27a7e3-e6b5
ARRIVY_AUTH_TOKEN=5730gWxBjDzbQDEeFh3zrs
ARRIVY_COMPANY_NAME=your_company_name
ARRIVY_WEBHOOK_SECRET=generated_secret
ARRIVY_BASE_URL=https://app.arrivy.com/api
ARRIVY_RATE_LIMIT=30

# Verify setup
npm run dev
curl http://localhost:3000/api/webhooks/arrivy
```

---

**Need help?** See `VERIFICATION_CHANGES_SUMMARY.md` Comment 1 section for more details.

