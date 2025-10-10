# User Provisioning Guide

## Overview

The Kin Home Sales Dashboard uses an **invite-based user provisioning system** to ensure only users who need access get accounts. This approach prevents thousands of inactive accounts while maintaining proper access control.

### Three Provisioning Methods

1. **Invite-Based (Recommended)** - Admin sends invite → User receives email → User sets password → Account activated
2. **QuickBase Lookup** - Admin searches QuickBase → Selects user → Account created with auto-filled data
3. **Smart Sync** - Bulk sync users with recent projects (last N months)

### Why Invite-Based is Recommended

- **Security**: Only creates accounts for users who actually need access
- **Efficiency**: Prevents thousands of inactive accounts from cluttering the system
- **Control**: Admins explicitly approve each new user
- **Compliance**: Clear audit trail of who was invited and when

### Office-Based Visibility

**Important**: Office-based roles (office_leader, area_director, divisional) see ALL projects in their assigned offices, regardless of whether closers/setters have accounts. This means you don't need to create accounts for every closer/setter - their projects are visible to managers through office-based visibility.

## Invite-Based Provisioning

### How It Works

1. **Admin sends invite** → System generates secure token and creates inactive user
2. **User receives email** (or admin shares link manually) → User clicks invite link
3. **User sets password** → Account activated and QuickBase data auto-synced
4. **User can log in** → Full access to dashboard based on role

### Step-by-Step Guide

#### For Admins (Creating Invites)

1. **Navigate to Settings → Users**
2. **Click "Invite User" button**
3. **Fill in user details:**
   - Email address (must be unique)
   - Full name
   - Role (closer, setter, office_leader, area_director, divisional, team_lead, super_admin)
4. **For office-based roles, select offices:**
   - Office Leader: Select single office
   - Area Director: Select multiple offices in region
   - Divisional: Select multiple offices across regions
5. **Choose email delivery:**
   - ✅ **Send email invitation** (if email is configured)
   - ❌ **Share link manually** (copy invite link to share via Slack, text, etc.)
6. **Click "Send Invite"**
7. **Share invite link** (if email not sent automatically)

#### For Users (Accepting Invites)

1. **Receive invite email** (or get link from admin)
2. **Click invite link** → Opens accept invite page
3. **Verify your information** (name, role, office)
4. **Set your password** (minimum 8 characters)
5. **Click "Accept Invite"** → Account activated
6. **Auto-login** → Redirected to dashboard

### Email vs Manual Delivery

#### Email Delivery (Recommended)
- **Automatic**: User receives professional email with invite link
- **Professional**: Branded email with role-specific instructions
- **Secure**: Link expires in 7 days for security
- **Convenient**: One-click access to accept invite

#### Manual Delivery (Fallback)
- **Copy invite link** from admin interface
- **Share via Slack, text, or in-person**
- **Same security**: Link still expires in 7 days
- **Same process**: User clicks link and sets password

### Invite Expiration

- **7-day expiration** for security reasons
- **Cannot be extended** - create new invite if expired
- **Clear error message** if user tries to use expired link
- **Admin can revoke** unused invites (super admin only)

### Revoking Invites

**Super admins can revoke unused invites:**

1. Go to Settings → Users
2. Find user with pending invite
3. Click "Revoke Invite" button
4. Confirm deletion
5. User account is deleted (since invite wasn't accepted)

**Cannot revoke accepted invites** - use deactivate user instead.

## QuickBase Lookup (Future Phase)

### How It Works

1. **Admin searches QuickBase** → Enter email or name
2. **Select from results** → Choose correct user
3. **Account created** → Auto-filled with QuickBase data
4. **User can log in** → Uses existing QuickBase email/password

### When to Use

- **Specific users** who need immediate access
- **Existing QuickBase users** with known credentials
- **Emergency access** when invite system is down

### Step-by-Step Guide (To Be Implemented)

1. Navigate to Settings → Users
2. Click "QuickBase Lookup" button
3. Search by email or name
4. Select correct user from results
5. Review auto-filled data
6. Click "Create Account"
7. User can log in immediately

## Smart Sync (Future Phase)

### How It Works

1. **Bulk sync** users with recent projects (last 3/6/12 months)
2. **Activity filtering** → Only syncs users with project activity
3. **Auto-creation** → Creates accounts for active users
4. **Batch processing** → Handles large numbers efficiently

### When to Use

- **Initial setup** → Bulk import existing active users
- **Periodic updates** → Sync new active users
- **Office onboarding** → Import entire office teams

### Activity Filtering

- **3 months**: Recent activity (recommended for active teams)
- **6 months**: Medium activity (good for seasonal businesses)
- **12 months**: Historical activity (comprehensive but may include inactive users)

### Step-by-Step Guide (To Be Implemented)

1. Navigate to Settings → Users
2. Click "Smart Sync" button
3. Select activity threshold (3/6/12 months)
4. Select offices to sync (optional)
5. Review preview of users to be created
6. Click "Start Sync" → Batch processing begins
7. Monitor progress and results

## Who Needs an Account

### ✅ YES - Create Accounts For:

#### Managers (All Levels)
- **Office Leaders** → Need to see ALL projects in their office
- **Area Directors** → Need to see projects across multiple offices
- **Divisional Managers** → Need to see projects across regions
- **Team Leads** → Need to see projects for their managed reps

#### Active Sales Reps
- **Active Closers** → Need to track their assigned projects
- **Active Setters** → Need to track their assigned projects
- **High-performing reps** → Need access for motivation and tracking

### ❌ NO - Don't Create Accounts For:

#### Inactive Sales Reps
- **Inactive closers/setters** → Their projects are visible to managers
- **Historical reps** → Old projects are visible to managers
- **Seasonal workers** → Only create if they'll be active soon
- **Former employees** → Their projects remain visible to managers

#### Support Staff
- **Customer service** → Don't need project access
- **Accounting** → Don't need project access
- **IT support** → Don't need project access

### Office-Based Visibility Explained

**Key Point**: Managers see ALL projects in their offices, regardless of whether closers/setters have accounts.

**Example**:
- Office Leader "Sarah" has account
- Closer "John" does NOT have account
- Sarah can see ALL projects in her office, including John's projects
- Sarah can manage holds, track progress, and oversee John's work
- John's projects appear in Sarah's dashboard with his name as the closer

**This means**:
- You don't need accounts for every closer/setter
- Focus on creating accounts for managers and active reps
- Use office-based visibility to oversee inactive reps' projects

## Troubleshooting

### Common Issues

#### Invite Email Not Received
- **Check spam folder** → Email may be filtered
- **Verify email address** → Typos prevent delivery
- **Check server logs** → Look for email send errors
- **Try manual delivery** → Copy invite link and share directly

#### Invite Link Expired
- **Create new invite** → Old links cannot be extended
- **Check invite date** → Links expire after 7 days
- **Verify token** → Ensure link wasn't corrupted

#### User Already Exists
- **Cannot send invite** → Email must be unique
- **Check existing users** → User may already have account
- **Reset password** → Use password reset instead of invite

#### Email Not Configured
- **App still works** → Invites can be shared manually
- **Configure email** → See Email Configuration section
- **Use manual delivery** → Copy invite links to share

### Error Messages

#### "Invalid or expired invite token"
- **Solution**: Create new invite (old link expired)
- **Prevention**: Use invites within 7 days

#### "Invite has already been used"
- **Solution**: User already accepted invite
- **Check**: User should be able to log in

#### "User already exists with this email"
- **Solution**: User already has account
- **Action**: Use password reset or check existing user

#### "Email not configured"
- **Solution**: Configure email provider or use manual delivery
- **Workaround**: Copy invite link and share manually

## Best Practices

### Provisioning Strategy

#### Start with Managers
1. **Create accounts for all office leaders** → They need to see all projects
2. **Create accounts for area directors** → They need regional oversight
3. **Create accounts for divisional managers** → They need strategic oversight

#### Add Active Sales Reps
1. **Identify active closers/setters** → Those with recent project activity
2. **Create accounts for high performers** → Motivation and tracking
3. **Create accounts for new hires** → Immediate access for training

#### Use Smart Sync for Bulk Import
1. **Run Smart Sync with 6-month threshold** → Good balance of activity
2. **Review imported users** → Remove any inactive accounts
3. **Set up regular sync schedule** → Monthly or quarterly updates

### Account Management

#### Regular Reviews
- **Quarterly user audit** → Review active vs inactive users
- **Deactivate inactive users** → Keep system clean
- **Monitor login activity** → Identify unused accounts

#### Security Practices
- **Use strong passwords** → Enforce password requirements
- **Regular password resets** → Quarterly or semi-annual
- **Monitor failed logins** → Watch for security issues

#### Training and Support
- **User onboarding checklist** → Ensure proper training
- **Documentation access** → Provide help resources
- **Support contact** → Clear escalation path

### Office-Based Access

#### Office Assignment Strategy
- **Assign users to correct offices** → Ensures proper project visibility
- **Use multiple offices for managers** → Area directors and divisionals
- **Regular office reviews** → Update assignments as needed

#### Project Visibility
- **Managers see all projects** → In their assigned offices
- **Reps see their projects** → Where they're listed as closer/setter
- **Admin sees everything** → Full system access

## Integration with QuickBase

### Auto-Sync on Account Activation

When a user accepts an invite:

1. **System searches QuickBase** → Using email address
2. **Auto-fills user data** → QuickBase user ID, phone, office
3. **Updates user record** → Backfills missing information
4. **Logs sync activity** → Audit trail of data backfill

### Data Mapping

- **Email** → Primary identifier for QuickBase lookup
- **QuickBase User ID** → Stored for future API calls
- **Phone** → Auto-filled from QuickBase if available
- **Office** → Auto-filled from QuickBase if not already set

### Error Handling

- **QuickBase lookup fails** → Account still activates
- **Missing QuickBase data** → User can update profile later
- **Multiple QuickBase matches** → Uses best match by email
- **No QuickBase match** → Account activates with invite data only

## Compliance and Audit

### Audit Trail

All provisioning activities are logged:

- **Invite creation** → Who invited whom, when, with what role
- **Invite acceptance** → When user activated account
- **QuickBase sync** → What data was backfilled
- **Account changes** → Role updates, office assignments
- **Account deactivation** → When and why accounts were disabled

### Compliance Features

- **Immutable audit logs** → Cannot be edited or deleted
- **User attribution** → Every action tied to specific user
- **Data retention** → Logs kept according to policy
- **Export capabilities** → CSV export for compliance reporting

### Security Considerations

- **Token-based invites** → Secure, time-limited access
- **Password requirements** → Minimum 8 characters
- **Account expiration** → Inactive accounts can be deactivated
- **Role-based access** → Users only see what they need

## Support and Maintenance

### Regular Tasks

#### Weekly
- **Monitor invite emails** → Check for delivery issues
- **Review new users** → Ensure proper role assignments
- **Check failed logins** → Watch for security issues

#### Monthly
- **User activity review** → Identify inactive accounts
- **Office assignment audit** → Ensure correct assignments
- **Email configuration check** → Verify email provider status

#### Quarterly
- **Bulk user sync** → Run Smart Sync for new active users
- **Security review** → Audit user access and permissions
- **Documentation update** → Keep guides current

### Support Contacts

- **Technical issues** → IT support team
- **User access problems** → System administrator
- **Email delivery issues** → Email provider support
- **QuickBase integration** → QuickBase administrator

### Maintenance Windows

- **User provisioning** → Available 24/7
- **Bulk operations** → Schedule during low-usage periods
- **System updates** → Coordinate with business hours
- **Email provider maintenance** → Monitor provider status pages

## Future Enhancements

### Planned Features

#### Phase 2: Enhanced Provisioning
- **QuickBase Lookup UI** → Search and select from QuickBase
- **Smart Sync Interface** → Bulk import with activity filtering
- **Advanced filtering** → More granular sync options

#### Phase 3: Automation
- **Auto-invite rules** → Automatic invites based on criteria
- **Integration webhooks** → Real-time user provisioning
- **Advanced analytics** → User activity and engagement metrics

#### Phase 4: Self-Service
- **Self-registration** → Users can request access
- **Approval workflows** → Manager approval for access requests
- **Role-based self-service** → Limited self-management capabilities

### Integration Opportunities

#### HR Systems
- **Employee onboarding** → Automatic account creation
- **Role synchronization** → Keep roles in sync with HR
- **Departure processing** → Automatic account deactivation

#### Active Directory
- **Single sign-on** → Use existing AD credentials
- **Group synchronization** → Sync AD groups to roles
- **Centralized management** → Manage users in AD

#### Communication Platforms
- **Slack integration** → Notify teams of new users
- **Microsoft Teams** → Integration with Teams workflows
- **Email automation** → Enhanced email workflows

---

## Quick Reference

### Invite Creation Checklist
- [ ] User email is unique
- [ ] Role is appropriate for user's responsibilities
- [ ] Office assignments are correct
- [ ] Email is configured (or use manual delivery)
- [ ] Invite link is shared with user

### User Acceptance Checklist
- [ ] User received invite (email or manual link)
- [ ] User clicked invite link
- [ ] User verified their information
- [ ] User set secure password
- [ ] User can log in to dashboard
- [ ] User sees appropriate projects

### Troubleshooting Checklist
- [ ] Check spam folder for invite email
- [ ] Verify email address is correct
- [ ] Ensure invite hasn't expired (7 days)
- [ ] Check server logs for errors
- [ ] Verify email configuration
- [ ] Try manual link sharing

### Best Practices Checklist
- [ ] Start with manager accounts
- [ ] Add active sales reps
- [ ] Use office-based visibility for oversight
- [ ] Regular user activity reviews
- [ ] Quarterly bulk sync operations
- [ ] Monitor audit logs for compliance

