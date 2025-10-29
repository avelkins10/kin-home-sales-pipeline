# ⚠️ CRITICAL SECURITY ALERT

## Immediate Action Required

**Real credentials were found in documentation files and must be rotated immediately.**

---

## Step 1: Rotate All Exposed Credentials (IMMEDIATE)

### Arrivy Credentials
1. Log into https://app.arrivy.com/
2. Go to Settings → Integrations → API Keys
3. **Revoke** existing credentials:
   - Auth Key: `0a27a7e3-e6b5` ❌ COMPROMISED
   - Auth Token: `5730gWxBjDzbQDEeFh3zrs` ❌ COMPROMISED
4. **Generate** new credentials
5. Update `.env.local` with new values
6. Update Vercel environment variables:
   ```bash
   vercel env rm ARRIVY_AUTH_KEY production
   vercel env rm ARRIVY_AUTH_TOKEN production
   vercel env add ARRIVY_AUTH_KEY production
   vercel env add ARRIVY_AUTH_TOKEN production
   ```

### Webhook Secret
1. Generate new webhook secret:
   ```bash
   openssl rand -base64 32
   ```
2. Update `.env.local`:
   ```bash
   ARRIVY_WEBHOOK_SECRET=<new_secret_here>
   ```
3. Update Arrivy webhook configuration with new secret
4. Update Vercel:
   ```bash
   vercel env rm ARRIVY_WEBHOOK_SECRET production
   vercel env add ARRIVY_WEBHOOK_SECRET production
   ```

### Other Exposed Credentials
Check and rotate if exposed:
- `RESEND_API_KEY`
- `REPCARD_API_KEY`
- QuickBase tokens (`QUICKBASE_TOKEN`)
- Database credentials in `DATABASE_URL`

---

## Step 2: Verify .env.local is Gitignored

```bash
# Check if .env.local is in .gitignore
grep "\.env.*\.local" .gitignore

# Expected output:
# .env*.local
# .env.local

# Verify .env.local is not tracked
git status .env.local
# Expected: "fatal: pathspec '.env.local' did not match any files"
```

If `.env.local` appears in `git status`:
```bash
# Remove from tracking
git rm --cached .env.local
git commit -m "Remove .env.local from version control"
```

---

## Step 3: Check Git History for Leaked Secrets

```bash
# Search commit history for exposed credentials
git log --all --full-history --source --pretty=format:'%H' -- .env.local

# If .env.local was ever committed, secrets may be in history
```

### If Secrets Are in Git History:

**Option A: Use BFG Repo-Cleaner (Recommended)**
```bash
# Install BFG
brew install bfg  # macOS
# or download from: https://rtyley.github.io/bfg-repo-cleaner/

# Backup your repo first
cd ..
cp -r Rep_Dashboard Rep_Dashboard_backup

# Remove .env.local from history
cd Rep_Dashboard
bfg --delete-files .env.local
git reflog expire --expire=now --all
git gc --prune=now --aggressive

# Force push (WARNING: This rewrites history)
git push --force
```

**Option B: GitHub Secret Scanning**
1. Go to your GitHub repo → Settings → Security → Secret scanning
2. Review and revoke any detected secrets
3. Follow GitHub's remediation steps

---

## Step 4: Use Secret Management Best Practices

### For Local Development
```bash
# Create .env.local from template
cp env.example .env.local

# Generate secure secrets
openssl rand -base64 32 > webhook_secret.txt

# NEVER commit .env.local
echo ".env.local" >> .gitignore
```

### For Production (Vercel)
```bash
# Always use Vercel's secret management
vercel env add SECRET_NAME production

# Verify secrets are set
vercel env ls

# Pull secrets for local testing (but don't commit!)
vercel env pull .env.production.local
echo ".env.production.local" >> .gitignore
```

### For Team Sharing
- Use password managers (1Password, LastPass) for sharing
- Use encrypted vault files (git-crypt, transcrypt)
- **NEVER** share via Slack, email, or commit to repo

---

## Step 5: Update Documentation

All documentation files have been updated to use placeholders:
- ✅ `ARRIVY_EXTERNAL_CONFIGURATION.md`
- ✅ `ARRIVY_PRODUCTION_DEPLOYMENT.md`
- ✅ `ARRIVY_TESTING_CHECKLIST.md`

Real credentials are replaced with:
- `<your_arrivy_auth_key_from_dashboard>`
- `<your_arrivy_auth_token_from_dashboard>`
- `<your_webhook_secret_from_env_local>`
- `$ARRIVY_AUTH_KEY` (environment variable reference)
- `$ARRIVY_AUTH_TOKEN` (environment variable reference)

---

## Step 6: Security Checklist

- [ ] Arrivy API credentials rotated
- [ ] Webhook secret regenerated
- [ ] Other exposed credentials rotated
- [ ] `.env.local` removed from git tracking
- [ ] `.env.local` listed in `.gitignore`
- [ ] Git history cleaned (if needed)
- [ ] Vercel environment variables updated
- [ ] Arrivy webhook updated with new secret
- [ ] Documentation updated with placeholders
- [ ] Team notified of credential rotation

---

## Prevention Measures

### Pre-commit Hook
Create `.git/hooks/pre-commit`:
```bash
#!/bin/bash
if git diff --cached --name-only | grep -q ".env.local"; then
  echo "ERROR: Attempting to commit .env.local"
  echo "This file contains secrets and must not be committed."
  exit 1
fi

if git diff --cached | grep -E "(ARRIVY_AUTH_KEY|ARRIVY_AUTH_TOKEN|ARRIVY_WEBHOOK_SECRET)" | grep -v "<"; then
  echo "ERROR: Real credentials detected in staged files"
  echo "Use placeholders like <your_secret_here> in documentation."
  exit 1
fi
```

```bash
chmod +x .git/hooks/pre-commit
```

### GitHub Secret Scanning
Enable in repository settings:
1. Go to Settings → Security → Code security and analysis
2. Enable "Secret scanning"
3. Enable "Push protection"

---

## Contact

If you believe credentials were exposed publicly:
1. **Immediately** revoke all credentials
2. Review access logs for unauthorized usage
3. Notify security team
4. Consider changing related passwords
5. Enable 2FA on all accounts

---

## Additional Resources

- [GitHub: Removing sensitive data](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/removing-sensitive-data-from-a-repository)
- [Vercel: Environment Variables](https://vercel.com/docs/concepts/projects/environment-variables)
- [OWASP: Credential Scanning](https://owasp.org/www-community/vulnerabilities/Use_of_hard-coded_password)

---

**Priority:** 🚨 CRITICAL  
**Impact:** HIGH - Exposed credentials can lead to unauthorized access  
**Action:** Rotate all credentials immediately

