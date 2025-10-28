# Arrivy Environment Setup Instructions

## ⚠️ IMPORTANT: `.env.local` File Creation Required

The `.env.local` file is gitignored and must be created manually to protect sensitive credentials.

## Quick Setup (2 minutes)

### Step 1: Copy the environment template
```bash
cd /Users/austinelkins/Rep_Dashboard
cp env.example .env.local
```

### Step 2: Add Arrivy credentials

Open `.env.local` in your editor and update the Arrivy section:

```bash
# =============================================================================
# ARRIVY CONFIGURATION
# =============================================================================

# Already provided credentials:
ARRIVY_AUTH_KEY=0a27a7e3-e6b5
ARRIVY_AUTH_TOKEN=5730gWxBjDzbQDEeFh3zrs

# TODO: Set your company name (as it appears in Arrivy dashboard)
# This is used for tracker URLs: https://app.arrivy.com/live/track/{company_name}/{url_safe_id}
ARRIVY_COMPANY_NAME=your_company_name_here

# TODO: Generate webhook secret using this command:
# openssl rand -base64 32
# Then paste the output here:
ARRIVY_WEBHOOK_SECRET=your_generated_webhook_secret_here

# These can stay as default:
ARRIVY_BASE_URL=https://app.arrivy.com/api
ARRIVY_RATE_LIMIT=30
```

### Step 3: Generate webhook secret
```bash
# Run this command and copy the output:
openssl rand -base64 32

# Example output: 
# x8K3mN9pL2qR5tY7vZ1aC4bD6eF8gH0jK2lM4nP6rS8=

# Paste this into ARRIVY_WEBHOOK_SECRET in .env.local
```

### Step 4: Find your Arrivy company name

1. Log into https://app.arrivy.com/
2. Go to Settings → Company Profile
3. Look for "Company Name" or "URL Slug"
4. This is the name used in tracker URLs
5. Copy this name to `ARRIVY_COMPANY_NAME` in `.env.local`

### Step 5: Verify other required variables

Ensure these are already set in your `.env.local`:
- `DATABASE_URL` - Your PostgreSQL connection string
- `NEXTAUTH_SECRET` - Authentication secret
- `NEXTAUTH_URL` - Your deployment URL (http://localhost:3000 for dev)
- `QUICKBASE_TOKEN` - QuickBase API token
- `QUICKBASE_REALM` - QuickBase realm (kin.quickbase.com)

### Step 6: Test configuration
```bash
# Start development server
npm run dev

# Check logs for successful Arrivy initialization:
# Expected: "[Arrivy] Client initialized successfully"
```

## Complete `.env.local` Template

Here's the complete Arrivy section for your `.env.local`:

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
ARRIVY_COMPANY_NAME=your_company_name

# Arrivy Webhook Secret (for webhook signature verification)
# Set this to a secure random string and configure the same in Arrivy webhook settings
# Generate with: openssl rand -base64 32
ARRIVY_WEBHOOK_SECRET=your_webhook_secret_here

# Arrivy API Base URL (default: https://app.arrivy.com/api)
# Only change this if you're using a custom Arrivy instance
ARRIVY_BASE_URL=https://app.arrivy.com/api

# Arrivy Rate Limit (requests per second)
# Arrivy API allows up to 30 requests per minute = 0.5 requests per second
# Default: 30 (requests per minute)
ARRIVY_RATE_LIMIT=30
```

## Verification Checklist

After creating `.env.local`, verify:

- [ ] File exists: `ls -la .env.local`
- [ ] File is gitignored: `git status` (should not show `.env.local`)
- [ ] All 6 Arrivy variables are set
- [ ] Webhook secret is at least 32 characters
- [ ] Company name matches Arrivy dashboard
- [ ] Auth key and token are correct
- [ ] No trailing spaces or quotes around values
- [ ] Other required variables (DATABASE_URL, NEXTAUTH_SECRET, etc.) are set

## For Production Deployment

When deploying to Vercel, set these as environment variables:

```bash
# Using Vercel CLI:
vercel env add ARRIVY_AUTH_KEY production
vercel env add ARRIVY_AUTH_TOKEN production
vercel env add ARRIVY_COMPANY_NAME production
vercel env add ARRIVY_WEBHOOK_SECRET production
vercel env add ARRIVY_BASE_URL production
vercel env add ARRIVY_RATE_LIMIT production

# Or via Vercel Dashboard:
# https://vercel.com/your-project/settings/environment-variables
```

## Troubleshooting

### Error: "Arrivy not configured"
- Verify `.env.local` exists in project root
- Check that all ARRIVY_* variables are set
- Restart development server: `npm run dev`

### Error: "Invalid API credentials"
- Verify `ARRIVY_AUTH_KEY` = `0a27a7e3-e6b5`
- Verify `ARRIVY_AUTH_TOKEN` = `5730gWxBjDzbQDEeFh3zrs`
- Test credentials directly:
```bash
curl -X GET https://app.arrivy.com/api/tasks \
  -H "X-Auth-Key: 0a27a7e3-e6b5" \
  -H "X-Auth-Token: 5730gWxBjDzbQDEeFh3zrs"
```

### Error: "Tracker URL not working"
- Verify `ARRIVY_COMPANY_NAME` matches exactly in Arrivy dashboard
- Company name is case-sensitive
- No spaces or special characters (use hyphens if needed)

### Webhook signature verification fails
- Verify webhook secret in `.env.local` matches Arrivy webhook settings
- Regenerate secret if needed (must update both places)
- Ensure no extra whitespace in `.env.local`

## Security Notes

⚠️ **NEVER commit `.env.local` to version control!**

- `.env.local` is already in `.gitignore`
- Contains sensitive API credentials
- Each developer should create their own `.env.local`
- Use different webhook secrets for dev/staging/production
- Rotate credentials if accidentally exposed

## Next Steps

After creating `.env.local`:

1. ✅ Run database migration (see `ARRIVY_DEPLOYMENT_GUIDE.md` Phase 2)
2. ✅ Test task creation locally
3. ✅ Configure webhook in Arrivy dashboard
4. ✅ Deploy to production with environment variables
5. ✅ Complete full testing suite

---

**Need help?** Refer to `ARRIVY_DEPLOYMENT_GUIDE.md` for complete deployment instructions.

