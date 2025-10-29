/**
 * Script to trigger RepCard sync via API
 * This requires you to be logged in as super_admin
 */

// This script should be run from browser console after logging in
// Or use curl with your session cookie

console.log(`
To trigger RepCard sync, run this in your browser console while logged in:

async function triggerSync() {
  const response = await fetch('/api/admin/repcard/comprehensive-sync?skipCustomerAttachments=true&skipAppointmentAttachments=true', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    }
  });
  
  const data = await response.json();
  console.log('Sync result:', data);
  return data;
}

triggerSync();
`);

