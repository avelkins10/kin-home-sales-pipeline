# Quickbase Token Rotation Process

**Purpose:** Zero-downtime procedure for rotating Quickbase API tokens.

**Frequency:** Every 90 days (recommended) or immediately after security incident

---

## Why Rotate Tokens?

- **Security Best Practice:** Limits exposure window if token is compromised
- **Compliance:** May be required by security policies
- **Incident Response:** Immediately revoke access if token is leaked

---

## Rotation Schedule

**Planned Rotation:** Every 90 days on the 1st of the quarter
- Q1: January 1
- Q2: April 1
- Q3: July 1
- Q4: October 1

**Emergency Rotation:** Immediately if:
- Token is accidentally committed to version control
- Token is shared in insecure channel (email, Slack, etc.)
- Suspicious API activity detected
- Team member with token access leaves company
- Security audit recommends rotation

---

## Zero-Downtime Rotation Procedure

**Overview:** Generate new token, update Vercel, verify, then revoke old token.

### Step 1: Generate New Token

**1.1 Login to Quickbase:**
- Navigate to https://kin.quickbase.com
- Login with admin account

**1.2 Generate Token:**
- Click profile icon → "My Preferences"
- Navigate to "My User Token" section
- Click "Generate New Token"
- **IMPORTANT:** Copy token immediately (you won't see it again!)
- Save token securely (password manager, encrypted note)

**1.3 Test New Token:**

Test locally before deploying:

```bash
# Update .env.local with new token
QUICKBASE_TOKEN=new-token-here

# Run health check
npm run setup:health

# Verify Quickbase API check passes
```

If health check fails, verify token has correct permissions.

### Step 2: Update Production Environment

**2.1 Update Vercel Environment Variable:**

**Option A: Via Vercel Dashboard (Recommended)**
1. Go to Vercel project → Settings → Environment Variables
2. Find `QUICKBASE_TOKEN`
3. Click "Edit"
4. Paste new token value
5. Select "Production" environment only (update Preview/Dev separately)
6. Click "Save"
7. **DO NOT redeploy yet** (old token still works)

**Option B: Via Vercel CLI**
```bash
vercel env rm QUICKBASE_TOKEN production
vercel env add QUICKBASE_TOKEN production
# Paste new token when prompted
```

**2.2 Trigger Deployment:**

**Option A: Via Vercel Dashboard**
1. Go to Deployments tab
2. Click "Redeploy" on latest production deployment
3. Check "Use existing Build Cache" (faster)
4. Click "Redeploy"

**Option B: Via Git Push**
```bash
# Create empty commit to trigger deployment
git commit --allow-empty -m "chore: rotate Quickbase token"
git push origin main
```

**2.3 Monitor Deployment:**
- Watch build logs in Vercel dashboard
- Verify build succeeds
- Verify deployment goes live
- Check runtime logs for any Quickbase API errors

### Step 3: Verify New Token Works

**3.1 Run Production Smoke Tests:**

```bash
BASE_URL=https://your-app.vercel.app npm run test:smoke
```

Verify all tests pass, especially:
- ✅ Dashboard loads with real data
- ✅ Projects list loads
- ✅ Project detail loads
- ✅ Hold update works

**3.2 Manual Verification:**
1. Login to production app
2. Navigate to dashboard
3. Verify metrics load (confirms Quickbase API works)
4. Navigate to projects list
5. Verify projects load
6. Open a project detail
7. Verify all data loads
8. Test hold update (place/release)
9. Verify update succeeds

**3.3 Check Logs:**
- Go to Vercel → Logs
- Filter for "Quickbase" or "QB"
- Verify no 401 errors (invalid token)
- Verify requests are succeeding

### Step 4: Revoke Old Token

**ONLY after verifying new token works!**

**4.1 Revoke in Quickbase:**
1. Go to Quickbase → My Preferences → My User Token
2. Find old token in list (if visible)
3. Click "Revoke" or "Delete"
4. Confirm revocation

**Note:** Quickbase may not show token list. If you can't revoke explicitly, the old token becomes inactive after generating a new one (verify this behavior with Quickbase support).

**4.2 Remove from Local Environment:**
- Update `.env.local` with new token
- Delete old token from password manager
- Update any documentation that referenced old token

**4.3 Update Preview/Development Environments:**

If using separate tokens for Preview/Dev:
1. Generate additional tokens in Quickbase
2. Update Vercel environment variables for Preview and Development
3. Redeploy preview branches if needed

---

## Rollback Procedure

**If new token doesn't work:**

**Immediate Rollback (if old token not revoked yet):**
1. Go to Vercel → Settings → Environment Variables
2. Edit `QUICKBASE_TOKEN`
3. Paste old token value
4. Save
5. Redeploy
6. Verify app works with old token
7. Investigate why new token failed

**If old token already revoked:**
1. Generate another new token in Quickbase
2. Test locally first
3. Update Vercel environment variable
4. Redeploy
5. Verify immediately

---

## Token Permissions Checklist

Verify new token has these permissions:

- [ ] Read access to Projects table (`br9kwm8na`)
- [ ] Write access to Projects table (for hold updates)
- [ ] Read access to Adders table (`bsaycczmf`)
- [ ] Read access to Tasks table (`br9kwm8q9`)
- [ ] Access to all required fields (92 fields)

**Test permissions:**
```bash
# Run health check with new token
npm run setup:health

# Should see:
# ✅ Quickbase API connected
# ✅ Sample project fetched
```

---

## Rotation Log

Document each rotation:

| Date | Rotated By | Reason | Old Token (last 4) | New Token (last 4) | Issues |
|------|------------|--------|-------------------|-------------------|--------|
| 2024-01-01 | [Name] | Scheduled | ...abc123 | ...def456 | None |
| 2024-04-01 | [Name] | Scheduled | ...def456 | ...ghi789 | None |

---

**✅ Token rotation complete - Next rotation due: [DATE]**
