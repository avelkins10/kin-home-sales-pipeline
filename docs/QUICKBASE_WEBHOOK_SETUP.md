# QuickBase Webhook Setup Guide

This guide explains how to configure QuickBase webhooks to automatically create notifications when new rep-visible notes are added to projects.

## Overview

When operations staff create a note in QuickBase and mark it as "Rep Visible", the system will:
1. Trigger a webhook to the Rep Dashboard
2. Verify the webhook signature for security
3. Determine which users should be notified (closer, setter, coordinator, office leaders, admins)
4. Create notifications for each recipient with appropriate priority
5. Display notifications in the notification bell for each user

## Prerequisites

- Admin access to QuickBase
- Deployed Rep Dashboard with publicly accessible URL
- `QUICKBASE_WEBHOOK_SECRET` configured in environment variables

## Step 1: Configure Environment Variable

Add the webhook secret to your production environment:

```bash
# In Vercel or your hosting platform
QUICKBASE_WEBHOOK_SECRET=7084855fa2409d7522bf4ddf0d4faeaa47829ae5d6da887687c7c6fa67b4da53
```

**Important:** This secret is used to verify that webhook requests are legitimately from QuickBase. Keep it secure and never commit it to version control.

## Step 2: Create Webhook in QuickBase

1. Navigate to QuickBase Notes Table (`bsb6bqt3b`)
2. Go to **Settings** → **Webhooks**
3. Click **Create New Webhook**

### Webhook Configuration

**Basic Settings:**
- **Name:** Rep Dashboard - New Rep Visible Note
- **Description:** Sends notifications when a rep-visible note is created

**Trigger:**
- **Event:** Record Created
- **Table:** Notes (bsb6bqt3b)
- **Filter:** `{141.EX.'Rep Visible'}` (only trigger when REP_VISIBLE field = "Rep Visible")

**Endpoint:**
- **URL:** `https://your-domain.com/api/webhooks/quickbase/notes`
  - Replace `your-domain.com` with your actual production domain
  - For Vercel: `https://kin-home-sales-pipeline.vercel.app/api/webhooks/quickbase/notes`
- **Method:** POST
- **Content Type:** application/json

**Security:**
- **Signing Method:** HMAC-SHA256
- **Signing Secret:** `7084855fa2409d7522bf4ddf0d4faeaa47829ae5d6da887687c7c6fa67b4da53`
  - Use the same secret from your environment variable
- **Signature Header:** `x-quickbase-signature`

**Payload:**
Include the following fields in the webhook payload:

```json
{
  "recordid": "{{record_id}}",
  "fieldChanges": {
    "3": {"value": "{{3}}"},
    "6": {"value": "{{6}}"},
    "7": {"value": "{{7}}"},
    "8": {"value": "{{8}}"},
    "9": {"value": "{{9}}"},
    "13": {"value": "{{13}}"},
    "141": {"value": "{{141}}"}
  }
}
```

**Field Mapping:**
- `3` = RECORD_ID (note ID)
- `6` = NOTE_CONTENT (note text)
- `7` = CATEGORY (Sales, Design, etc.)
- `8` = DATE_CREATED
- `9` = CREATED_BY (user object)
- `13` = RELATED_PROJECT (project record ID)
- `141` = REP_VISIBLE flag

## Step 3: Test the Webhook

### Option 1: Create Test Note in QuickBase

1. Go to a project in QuickBase
2. Create a new note
3. Set category to "Sales" (critical priority)
4. Check "Rep Visible" checkbox
5. Save the note
6. Verify notifications appear in Rep Dashboard

### Option 2: Manual Test with cURL

```bash
curl -X POST https://your-domain.com/api/webhooks/quickbase/notes \
  -H "Content-Type: application/json" \
  -H "x-quickbase-signature: SIGNATURE_HERE" \
  -d '{
    "recordid": 12345,
    "fieldChanges": {
      "3": {"value": 12345},
      "6": {"value": "Test note content"},
      "7": {"value": "Sales"},
      "9": {"value": {"email": "test@kinhome.com", "name": "Test User"}},
      "13": {"value": 1001},
      "141": {"value": "Rep Visible"}
    }
  }'
```

**Note:** To generate the correct signature for testing:
```bash
echo -n 'PAYLOAD_JSON' | openssl dgst -sha256 -hmac '7084855fa2409d7522bf4ddf0d4faeaa47829ae5d6da887687c7c6fa67b4da53' -hex
```

### Option 3: Development Testing (No Signature Required)

In development mode (`NODE_ENV=development`), the webhook accepts requests without signatures for easier testing:

```bash
curl -X POST http://localhost:3000/api/webhooks/quickbase/notes \
  -H "Content-Type: application/json" \
  -d '{
    "recordid": 12345,
    "fieldChanges": {
      "6": {"value": "Test note content"},
      "7": {"value": "Sales"},
      "9": {"value": {"email": "test@kinhome.com", "name": "Test User"}},
      "13": {"value": 1001},
      "141": {"value": "Rep Visible"}
    }
  }'
```

## Step 4: Verify Webhook Logs

Check webhook execution in QuickBase:
1. Go to **Settings** → **Webhooks**
2. Click on your webhook
3. View **Execution History**
4. Verify successful (200/201) responses

Check Rep Dashboard logs:
```bash
# View Vercel logs or server logs
# Look for entries like:
[WEBHOOK] Received QuickBase note webhook: { recordid: 12345, ... }
[WEBHOOK] Creating notifications for recipients: { count: 5, ... }
```

## Priority Configuration

Note categories are mapped to priorities in `lib/quickbase/notePriority.ts`:

### Critical Priority (Red Badge)
Urgent issues requiring immediate attention:
- Sales
- Acceptance
- Installation
- Inspection
- PTO

### Normal Priority (Blue Badge)
Standard project updates:
- Survey
- Design
- NEM
- Permitting
- HOA
- Verification
- Commissioning

### Info Priority (Gray Badge)
Low-priority informational notes (currently none configured, but available for custom categories)

**To adjust priorities:** Edit `NOTE_PRIORITY_CONFIG` in `lib/quickbase/notePriority.ts`

## Recipient Rules

Notifications are sent to the following users:

1. **Project Team** (always):
   - Closer (required)
   - Setter (if assigned)
   - Project Coordinator (if assigned)

2. **Office Leaders**:
   - All office leaders assigned to the project's sales office

3. **Admins**:
   - All users with `super_admin` or `regional` roles

**Note:** Users are automatically deduplicated if they match multiple criteria (e.g., a closer who is also an office leader receives only one notification).

## Troubleshooting

### Webhook Not Firing
- Verify the webhook filter: `{141.EX.'Rep Visible'}`
- Check that "Rep Visible" checkbox is checked on the note
- Verify webhook is enabled in QuickBase

### Signature Verification Failed
- Ensure `QUICKBASE_WEBHOOK_SECRET` matches in both QuickBase and your environment
- Check that signature header is named `x-quickbase-signature`
- Verify HMAC-SHA256 is selected in QuickBase webhook settings

### No Notifications Created
- Check that project exists and has valid team assignments
- Verify users table has correct email addresses
- Check server logs for errors in recipient determination

### Missing Recipients
- Verify user roles in database (`users` table)
- Check office assignments for office leaders
- Ensure project team fields (closer, setter, coordinator) are populated in QuickBase

## Webhook Endpoint Details

**URL:** `/api/webhooks/quickbase/notes`

**Method:** POST

**Authentication:** HMAC-SHA256 signature verification

**Rate Limiting:** No built-in rate limiting (relies on QuickBase webhook throttling)

**Response Codes:**
- `201` - Notifications created successfully
- `200` - Webhook processed (but may have been skipped)
- `400` - Invalid payload or missing required fields
- `401` - Invalid or missing signature
- `500` - Server error

**Response Body:**
```json
{
  "success": true,
  "notificationsCreated": 5,
  "recipients": 5
}
```

## Security Considerations

1. **Signature Verification**: All production webhooks must include valid HMAC-SHA256 signatures
2. **Secret Management**: Never commit `QUICKBASE_WEBHOOK_SECRET` to version control
3. **HTTPS Only**: Webhook endpoint should only be accessible via HTTPS in production
4. **Input Validation**: All webhook data is validated before processing
5. **Error Handling**: Failed notifications for individual users don't prevent processing for others

## Monitoring

Key metrics to monitor:
- Webhook execution success rate (in QuickBase logs)
- Number of notifications created per webhook
- Time to process webhook (should be < 5 seconds)
- Failed signature verifications (potential security issue)

Set up alerts for:
- Repeated signature verification failures
- Webhook errors > 5% of requests
- No webhooks received for > 24 hours (potential integration issue)
