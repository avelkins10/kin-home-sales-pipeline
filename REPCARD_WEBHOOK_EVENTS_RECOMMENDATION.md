# RepCard Webhook Events - Recommendation

## Quick Answer: Which Events to Use?

**Minimum (Must Have):**
1. ✅ **Appointment Set** - This is THE event you need for real-time appointment visibility
2. ✅ **New Contact** - Ensures customers are synced so appointments can link properly
3. ✅ **Door knocked** - Same as New Contact (RepCard may send both, so subscribe to both)

**Recommended (Add These):**
4. ✅ **Appointment Update** - When appointment time/details change
5. ✅ **Appointment Outcome** - When appointment is closed/no-show/cancelled
6. ✅ **Update Contact** - Keeps customer data current

**Skip These:**
- ❌ User events (New User, Update User, Remove User) - Handled by cron, not time-sensitive
- ❌ Contact Removed - Rare, not critical
- ❌ Status Changed - Nice to have but not critical
- ❌ Contact Type Changed - Rare, can skip

---

## Why These Events?

### Appointment Set (CRITICAL)
- **What it does:** Fires when a setter creates an appointment in RepCard
- **Why you need it:** This is the event that makes appointments appear in your app within 30-60 seconds instead of 2.5-5.5 minutes
- **Impact:** 10x faster visibility for new appointments

### New Contact / Door knocked (CRITICAL)
- **What it does:** Fires when a setter creates a new lead/customer
- **Why you need it:** Appointments need to link to customers. If the customer isn't synced yet, the appointment can't link properly, breaking metrics
- **Impact:** Ensures appointments can always link to their customers

### Appointment Update (IMPORTANT)
- **What it does:** Fires when appointment details change (time, location, closer, etc.)
- **Why you need it:** Keeps appointment data current in real-time
- **Impact:** Users see updated appointment times/details immediately

### Appointment Outcome (IMPORTANT)
- **What it does:** Fires when appointment disposition changes (closed, no-show, cancelled, etc.)
- **Why you need it:** Updates appointment outcomes in real-time for accurate metrics
- **Impact:** Leaderboards and metrics stay current

### Update Contact (IMPORTANT)
- **What it does:** Fires when customer info changes
- **Why you need it:** Keeps customer data (name, phone, address) current
- **Impact:** Ensures customer data accuracy

---

## Event Priority Summary

| Event | Priority | Reason |
|-------|----------|--------|
| **Appointment Set** | 🔴 CRITICAL | This is what makes appointments appear quickly |
| **New Contact** | 🔴 CRITICAL | Needed for appointment linking |
| **Door knocked** | 🔴 CRITICAL | Same as New Contact (subscribe to both) |
| **Appointment Update** | 🟡 IMPORTANT | Keeps appointment data current |
| **Appointment Outcome** | 🟡 IMPORTANT | Updates outcomes in real-time |
| **Update Contact** | 🟡 IMPORTANT | Keeps customer data current |
| **Status Changed** | 🟢 OPTIONAL | Nice to have, not critical |
| **Contact Type Changed** | 🟢 OPTIONAL | Rare, can skip |
| **Contact Removed** | 🟢 OPTIONAL | Rare, can skip |
| **User events** | ⚪ SKIP | Handled by cron, not time-sensitive |

---

## Recommended Configuration

**In RepCard Webhook Settings, select:**

✅ Appointment Set  
✅ Appointment Update  
✅ Appointment Outcome  
✅ New Contact  
✅ Update Contact  
✅ Door knocked  

**Skip:**
❌ All User events  
❌ Contact Removed  
❌ Status Changed (optional)  
❌ Contact Type Changed (optional)  

This gives you **real-time updates for everything that matters** while keeping the webhook simple and efficient.
