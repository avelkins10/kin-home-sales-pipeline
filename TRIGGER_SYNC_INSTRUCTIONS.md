# How to Trigger RepCard Sync

Since you're logged in, you can trigger the sync directly from the browser:

## Option 1: Admin Sync Page (Easiest)

1. Navigate to: `http://localhost:3000/admin/repcard-sync`
2. Click "Full Sync" or use the date range inputs
3. Wait for sync to complete

## Option 2: Browser Console (Quick)

While logged in, open browser console (F12) and run:

```javascript
async function triggerSync() {
  console.log('Starting RepCard sync...');
  
  const response = await fetch('/api/admin/repcard/comprehensive-sync?skipCustomerAttachments=true&skipAppointmentAttachments=true', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    }
  });
  
  const data = await response.json();
  console.log('Sync result:', data);
  
  if (data.success) {
    console.log('✅ Sync completed!');
    console.log('Users:', data.results.users);
    console.log('Offices:', data.results.offices);
    console.log('Customers:', data.results.customers);
    console.log('Appointments:', data.results.appointments);
    console.log('Status Logs:', data.results.statusLogs);
  } else {
    console.error('❌ Sync failed:', data);
  }
  
  return data;
}

triggerSync();
```

## Option 3: Direct API Call (curl)

```bash
# Get your session cookie from browser (Application → Cookies → next-auth.session-token)
# Then run:
curl -X POST "http://localhost:3000/api/admin/repcard/comprehensive-sync?skipCustomerAttachments=true&skipAppointmentAttachments=true" \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN_HERE" \
  -H "Content-Type: application/json"
```

## After Sync

Once sync completes:
1. Refresh the analytics page
2. Check the Canvassing tab - should show doors knocked, appointments, etc.
3. Check the Leaderboards tab - should show ranked users
4. Check the dashboard - RepCard Metrics card should show data

