# RepCard Webhook Payload Analysis

## What We're Currently Capturing

### From Appointment Webhooks:
- ✅ **IDs**: `id`, `appointment_id`, `contact.id` (customer), `user.id` (setter), `closer.id`
- ✅ **Timing**: `appt_start_time`, `appt_end_time`, `created_at`, `updated_at`, `duration`
- ✅ **Status**: `appointment_status_title`, `disposition`
- ✅ **Location**: `appointment_location`
- ✅ **Notes**: `notes`, `appointment_notes`
- ✅ **Full Payload**: Stored in `raw_data` JSONB column

### From Customer/Contact Webhooks:
- ✅ **IDs**: `id`, `contact_id`, `user.id` (setter)
- ✅ **Contact Info**: `name`, `firstName`, `lastName`, `email`, `phone`, `phoneNumber`
- ✅ **Address**: `address`, `city`, `state`, `zip`, `zipCode`
- ✅ **Status**: `status`, `statusId`
- ✅ **Timestamps**: `created_at`, `updated_at`
- ✅ **Full Payload**: Stored in `raw_data` JSONB column

## What's Available in Payload (From Database Analysis)

### Attachments/Files:
- ✅ **Appointment Attachment Fields**: `appointment_attachment`, `attachment`, `latestAttachment`, `soloAttachment`
- ✅ **Contact Source**: `contactSource` field available
- ⚠️ **Note**: Attachment fields may be null in payload - attachments likely synced separately via API

### Additional Useful Fields Available:
- ✅ **Appointment Link**: `appointmentLink`, `appointment_link` 
- ✅ **Reminder Info**: `remind_at`, `remind_text`
- ✅ **Timezone Info**: `start_at_timezone`, `end_at_timezone`, `startAtTimezone`, `endAtTimezone`
- ✅ **Contact Source**: `contactSource`
- ✅ **Appointment Title**: `title` (may be empty)
- ✅ **Location**: `appointmentLocation`, `appointment_location`, `latitude`, `longitude`
- ✅ **Pricing Info**: `gross_price`, `net_price`, `gross_ppw`, `net_ppw`, `system_size_kw`
- ✅ **QuickBase Link**: `qb_record_id`
- ✅ **Both Spouses Present**: `both_spouses_present`
- ✅ **Manager Info**: `manager` object
- ✅ **Owner Info**: `owner` object
- ✅ **Experience**: `experience` field

## Recommendations

1. **Check `raw_data` for attachment info**: The full payload is stored, so we can query it:
   ```sql
   SELECT raw_data->>'appointment_attachment' FROM repcard_appointments;
   SELECT raw_data->'contact'->>'attachments' FROM repcard_appointments;
   ```

2. **Sync attachments separately**: If attachments aren't in webhook payload, we should:
   - Sync attachments via API when processing webhooks
   - Or trigger attachment sync after customer/appointment webhook

3. **Extract additional fields**: If useful fields are in `raw_data`, we can:
   - Add columns for frequently-used fields
   - Or query `raw_data` directly when needed

## Next Steps

1. Query `raw_data` to see what attachment info is available
2. Check if we need to call attachment API endpoints after webhook processing
3. Identify any other useful fields we should extract
