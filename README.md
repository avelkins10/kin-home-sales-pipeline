# KINETIC Platform — Sales & Operations

## Phase Status Summary

Refer to `docs/IMPLEMENTATION-STATUS.md` for the authoritative status. CI/CD pipeline has been added; manual QA execution is in progress.

### Settings: System Configuration (Super Admin)
- ✅ SystemTab component with 4 configuration cards
- ✅ Quickbase Connection card with test connection button
- ✅ Connection status indicator (green: success, red: failed)
- ✅ Milestone SLA Defaults for all 7 milestones (survey, design, permit, NEM, install, inspection, PTO)
- ✅ Warning and critical threshold configuration (% of SLA)
- ✅ Hold Reasons management (add/remove custom reasons)
- ✅ General Settings (date format, timezone, session timeout)
- ✅ API routes: `/api/admin/system/settings` (GET/PUT), `/api/admin/system/test-connection` (POST)
- ✅ Zod validation for all system settings inputs
- ✅ Database schema update (system_settings table with JSONB storage)
- ✅ Audit logging stub for settings changes (preparation for Phase 5D)
- ✅ Optimistic UI updates with local state
- ✅ RBAC enforcement (super_admin only access)
- ✅ E2E tests for system settings CRUD and connection testing
- ✅ Unit tests for SLA validation and hold reason mutations

**System Configuration Features:**
- Super admins can configure Quickbase connection settings
- Super admins can test Quickbase connection before saving
- Super admins can set default SLA days for each milestone
- Super admins can configure warning/critical alert thresholds
- Super admins can manage available hold reason options
- Super admins can configure date format, timezone, and session timeout
- All settings saved together with single save action
- Settings stored in JSONB for flexible schema evolution

### System Configuration (Super Admin Only)

**System Tab:**
- Configure Quickbase API connection (realm, token)
- Test Quickbase connection before saving
- Set default SLA days for each of the 7 milestones
- Configure warning threshold (% of SLA for yellow alerts)
- Configure critical threshold (% of SLA for red alerts)
- Manage available hold reason options (add/remove)
- Configure date format (MM/DD/YYYY, DD/MM/YYYY, YYYY-MM-DD)
- Configure timezone (Eastern, Central, Mountain, Pacific)
- Configure session timeout (15-480 minutes)

**Access:** Only super_admin role can access this tab.

**SLA Defaults:**
- Survey: 7 days (range: 1-60)
- Design: 10 days (range: 1-60)
- Permit: 21 days (range: 1-90)
- NEM: 14 days (range: 1-60)
- Install: 7 days (range: 1-30)
- Inspection: 5 days (range: 1-30)
- PTO: 10 days (range: 1-60)

## Next Steps (Phase 5D): Audit Logs & Compliance

- Audit Logs (super admin): Compliance tracking, filters, pagination, CSV export, JSON diffs

Modern, iPad-first PWA for solar sales reps to track their projects through the 9-milestone installation process. Replaces clunky Quickbase interface with real-time visibility and offline support.

## Multi-App Architecture

The platform now supports both **Sales** and **Operations** apps within a single codebase:

- **Sales App** (`(sales)` route group): Existing sales features including projects, analytics, calendar, reports, and settings
- **Operations App** (`(operations)` route group): New operations features including work orders, inventory, scheduling, and quality control
- **App Switcher**: Top-left navbar component allows users with appropriate roles to toggle between apps
- **Shared Infrastructure**: Common auth, database, UI components, and offline support
- **Role-Based Access**: Users see different navigation and features based on their role and app context

### Route Group Structure
```
app/
├── (sales)/           # Sales app - existing dashboard features
│   ├── layout.tsx    # Sales app layout with data-app-context="sales"
│   ├── page.tsx      # Sales dashboard
│   ├── projects/     # Project management
│   ├── analytics/    # Sales analytics
│   └── settings/     # Sales settings
└── (operations)/     # Operations app - new operations features
    ├── layout.tsx    # Operations app layout with data-app-context="operations"
    ├── page.tsx      # Operations dashboard
    ├── work-orders/  # Work order management
    ├── inventory/    # Inventory management
    └── scheduling/   # Crew scheduling
```

## Tech Stack

- **Framework**: Next.js 14 (App Router) + TypeScript
- **Styling**: Tailwind CSS + shadcn/ui
- **State Management**: TanStack Query (React Query) + Zustand
- **Authentication**: NextAuth.js with credentials provider
- **Database**: Neon PostgreSQL
- **API Integration**: Quickbase REST API v1
- **Offline Support**: IndexedDB + Service Worker (Phase 3)

## Prerequisites

- Node.js 18+ and npm
- Neon PostgreSQL database (connection string provided)
- Quickbase user token (generate at kin.quickbase.com)

## Quick Setup

For detailed setup instructions, see **[SETUP.md](./SETUP.md)**.

### TL;DR - Fast Setup

```bash
# 1. Install dependencies
npm install

# 2. Configure environment
cp env.example .env.local
# Edit .env.local with your credentials (see SETUP.md for details)

# 3. Run automated setup
npm run setup:all

# 4. Start development
npm run dev
```

### Manual Setup (Step-by-Step)

If you prefer to run each step individually:

```bash
# Validate environment variables
npm run setup:env

# Initialize database (enable pgcrypto + run migrations)
npm run setup:db

# Seed test users
npm run setup:seed

# Run health check
npm run setup:health

# Start development server
npm run dev
```

## Project Structure

```
├── app/                    # Next.js App Router pages and API routes
│   ├── (auth)/            # Authentication pages
│   ├── (sales)/           # Sales app pages
│   ├── (operations)/       # Operations app pages
│   ├── api/               # API routes
│   └── globals.css        # Global styles
├── components/            # Reusable UI components
│   └── ui/               # shadcn/ui components
├── lib/                  # Business logic and utilities
│   ├── auth/             # NextAuth configuration
│   ├── constants/        # Field constants and configuration
│   ├── db/               # Database client and migrations
│   ├── quickbase/        # Quickbase API client and queries
│   ├── types/            # TypeScript type definitions
│   └── utils/            # Utility functions
├── docs/                 # Implementation guides and documentation
└── data/                 # Quickbase field configuration
```

## Key Features

### Email Configuration (Optional)

The invite system can send email invitations automatically. Email is **optional** - the app works without it by sharing invite links manually.

**Supported Providers:**
- **Resend** (recommended) - Simple setup, excellent deliverability
- **SendGrid** - Alternative for existing users

**Setup Steps:**
1. Choose a provider and sign up for an account
2. Verify your sender domain (required for production)
3. Get your API key from the provider dashboard
4. Add to `.env.local`:
   ```
   EMAIL_ENABLED=true
   MAIL_PROVIDER=resend
   RESEND_API_KEY=your_api_key_here
   MAIL_FROM=Kin Home Sales <no-reply@yourdomain.com>
   ```
5. Test by creating an invite with "Send email" checked

**Without Email:**
If email is not configured, admins can still create invites and share the invite link manually (copy/paste). The invite system works identically, just without automatic email delivery.

**Troubleshooting:**
- Check server logs for email send errors
- Verify sender domain is verified in your email provider
- Ensure API key has send permissions
- Test with a personal email first (not corporate email that may block)

### 9-Milestone Project Timeline
1. **Intake** - Project initiation and tentative install date
2. **Survey** - Site survey and approval
3. **Design** - System design and engineering
4. **HOA** - Homeowner association approval (conditional)
5. **Permit** - Building permit submission and approval
6. **NEM** - Net Energy Metering application
7. **Install** - System installation and commissioning
8. **Verification** - System verification (calculated)
9. **Inspection** - Final inspection
10. **PTO** - Permission to Operate (final milestone)

### Role-Based Access Control

**Sales Roles:**
- **Closers**: Projects where they are the closer
- **Setters**: Projects where they are the setter
- **Office Leaders**: Their projects + configurable office access
- **Regional**: Multiple offices (configurable)
- **Super Admin**: All projects + SLA configuration

**Operations Roles:**
- **Operations Coordinator**: Operations work orders and tasks assigned to them
- **Operations Manager**: Team-wide operations metrics and workflow management

**Cross-App Access:**
- **Office Leaders, Regional, Super Admin, Operations Manager**: Can access both Sales and Operations apps via app switcher
- **Sales-only roles** (closer, setter, team_lead): Access only Sales app
- **Operations-only roles** (operations_coordinator): Access only Operations app

**Note:** Operations features are under development and will be expanded based on user requirements. The current implementation provides the foundational structure and placeholder components.

### Real-Time Data
- Quickbase API integration with 10 req/sec rate limiting
- Request queuing and exponential backoff
- 30-second cache with React Query
- Optimistic updates for instant feedback

### User Provisioning
- 📧 Invite-based user provisioning with optional email delivery
- Role-based access control with office assignments
- QuickBase data auto-sync on account activation
- Secure token-based invite system with 7-day expiration

## Security

### Authentication & Authorization

The app uses NextAuth.js with credential-based authentication:

- **Password Security:** Bcrypt hashing with 10 salt rounds
- **Session Security:** JWT tokens with 30-day expiration
- **Cookie Security:** HTTPS-only, httpOnly, sameSite=lax in production
- **Role-Based Access:** 5 roles (closer, setter, office_leader, regional, super_admin)

### API Security

**Authorization Guards:**
```typescript
import { requireAuth, requireRole } from '@/lib/auth/guards'

// Require authentication
const auth = await requireAuth()
if (!auth.authorized) return auth.response

// Require specific role
const auth = await requireRole(['super_admin'])
if (!auth.authorized) return auth.response
```

**Protected Routes:**
- `/api/projects/[id]/hold` - Requires authentication
- All dashboard pages - Requires authentication (layout guard)

### Error Tracking (Optional)

To enable Sentry error tracking:

1. Install Sentry:
   ```bash
   npm install @sentry/nextjs
   ```

2. Set environment variables:
   ```bash
   SENTRY_DSN=your-server-dsn
   NEXT_PUBLIC_SENTRY_DSN=your-client-dsn
   ```

3. Restart app - Sentry will automatically capture errors

**Without Sentry:** App logs errors to console (available in Vercel logs)

### Security Audit

Run security audit:
```bash
npm run audit:prod        # Check production dependencies
npm run audit:fix         # Auto-fix vulnerabilities
npm run audit:report      # Generate JSON report
```

See `docs/SECURITY-READINESS.md` for detailed security documentation.

**CRITICAL: Environment Variables**
- The `.env.local` file contains sensitive credentials and is automatically ignored by git
- Never commit live tokens, database URLs, or secrets to version control
- Always use placeholder values in documentation and examples
- Generate fresh tokens for each environment (development, staging, production)
 - For audit logging to function, set `INTERNAL_API_SECRET` in all environments (local `.env.local`, CI secrets, and hosting provider env vars). Do not prefix with `NEXT_PUBLIC_`. The internal audit API at `/api/internal/audit` validates the `x-internal-secret` header against this value.

## Development

### Available Scripts

```bash
npm run dev          # Start development server
npm run build        # Build for production
npm run start        # Start production server
npm run lint         # Run ESLint
npm run type-check   # Run TypeScript compiler
```

## CI/CD Pipeline

GitHub Actions enforces quality gates on every PR and push:
- Type check, lint, unit tests (with coverage), E2E tests, build, audit
- Preview deploys for non-main branches
- Manual production deploy with smoke tests

See `docs/CI-CD-SETUP.md` for setup, secrets, and branch protection rules.

### Setup Scripts

```bash
npm run setup:env      # Validate .env.local configuration
npm run setup:db       # Initialize database schema
npm run setup:seed     # Create test users
npm run setup:health   # Validate all integrations
npm run setup:all      # Run all setup steps in sequence
```

See **[SETUP.md](./SETUP.md)** for detailed documentation.

### Data Integration

The app integrates with Quickbase using the official REST API v1 specification:

- **92 Field Constants**: All project fields mapped with usage percentages
- **Rate Limiting**: 10 requests/second with request queuing
- **Error Handling**: Exponential backoff for failed requests
- **Field Format**: All fields returned as `{ "fieldId": { "value": "data" } }`

### Critical Data Insights

**Low usage ≠ unreliable fields!** For example:
- PTO (20% usage) is the FINAL milestone - only completed projects have it
- HOA (10% usage) is CRITICAL for the 10% of homes that need it
- All fields ≥10% usage are included (42 fields for standard dashboard)

## Documentation

- **Implementation Guide**: `docs/TRAYCER-IMPLEMENTATION-BRIEF-ULTIMATE.md`
- **Field Usage Analysis**: `docs/UNDERSTANDING-FIELD-USAGE.md`
- **Field Selection Strategy**: `docs/FIELD-SELECTION-STRATEGY.md`
- **Quickbase API Reference**: `QuickBase_RESTful_API_2025-08-28T17_29_31.942Z.json`
- **CI/CD Setup**: `docs/CI-CD-SETUP.md`
- **QA Checklist**: `docs/QA-CHECKLIST.md`
- **Production Readiness**: `docs/PRODUCTION-READINESS.md`

## Phase 1 Status: ✅ COMPLETE

### Foundation Code
- ✅ Project setup and configuration
- ✅ Quickbase API client with rate limiting
- ✅ NextAuth authentication with role-based access
- ✅ Database schema and migrations
- ✅ 92 field constants defined
- ✅ Basic UI components and layouts
- ✅ Login page and dashboard structure

### Operational Setup (Phase 1 Hardening)
- ✅ Environment validation script (`setup:env`)
- ✅ Database initialization script (`setup:db`)
- ✅ User seeding script (`setup:seed`)
- ✅ Health check script (`setup:health`)
- ✅ Comprehensive setup documentation (`SETUP.md`)

## Phase 2 Status: ✅ COMPLETE

### Dashboard & Project List
- ✅ Dashboard with KPI tiles (active projects, holds, installs, pipeline value)
- ✅ Urgent alerts component showing top 3 holds
- ✅ Recent projects list with status badges
- ✅ Project list page with search, filters, sorting
- ✅ Project cards with milestone indicators and hold warnings
- ✅ Role-based data scoping (closer, setter, office leader, regional, admin)
- ✅ TanStack Query integration with 30s cache
- ✅ Responsive layout (iPad-first, desktop optimized)

## Phase 3 Status: ✅ COMPLETE

### Project Detail & Timeline
- ✅ Project detail page with comprehensive information display
- ✅ Customer contact card with call/text/email actions
- ✅ System specifications card with pricing breakdown
- ✅ Team members card with contact information
- ✅ Adders card with approval status
- ✅ 9-milestone timeline visualization with status indicators
- ✅ Comprehensive milestone status utilities with substeps and warnings
- ✅ Hold management UI with place/release/update capabilities
- ✅ API route for hold mutations with optimistic updates
- ✅ Vitest setup with unit tests for milestone utilities
- ✅ Dialog and Textarea UI components

## Phase 4C Status: ✅ COMPLETE

### Offline Support & PWA
- ✅ IndexedDB storage layer for caching projects (5-minute TTL)
- ✅ Mutation queue for offline hold updates with retry logic
- ✅ Offline-aware query wrappers with cache fallback
- ✅ Service worker with cache-first strategy for static assets
- ✅ PWA manifest with app metadata and icons
- ✅ Automatic sync when connectivity returns
- ✅ Optimistic UI updates for offline mutations
- ✅ Global offline indicator component
- ✅ Integration tests for offline scenarios (Playwright)
- ✅ Unit tests for IndexedDB storage layer (Vitest)

## Settings Phases

### Testing & Quality Assurance
- ✅ Expanded Vitest coverage for Quickbase client (rate limiting, queue, errors)
- ✅ Unit tests for offline sync queue (retry logic, online/offline events)
- ✅ Unit tests for traffic-lights utilities (7 milestone calculators)
- ✅ Unit tests for formatters, hold-detection, project-helpers
- ✅ E2E user journey test (login → dashboard → projects → detail → hold)
- ✅ E2E dashboard tests (metrics, alerts, navigation)
- ✅ E2E projects list tests (search, filters, traffic lights)
- ✅ Fixed failing offline tests (added data-testid attributes)
- ✅ Coverage reporting configured (HTML + LCOV)
- ✅ CI integration scripts (test:ci, test:all)
- ✅ QA handoff documentation with coverage links
- ✅ Coverage target achieved: >80% for lib/ directory

**Test Statistics:**
- Unit tests: 150+ test cases across 9 files
- E2E tests: 20+ test cases across 4 files
- Coverage: >80% for business logic (lib/)
- All tests passing on chromium + iPad

## Phase 5B Status: ✅ COMPLETE

### Settings: User Management (Super Admin)
- ✅ UsersTab component with user table, search, and filters
- ✅ Add new user dialog with role and office assignment
- ✅ Toggle user active/inactive status
- ✅ Reset user password (generates temporary password)
- ✅ Role badge color coding (super_admin: red, regional: blue, office_leader: gray, others: outline)
- ✅ OfficesTab component with office grid and stats
- ✅ Add new office dialog with region and leader selection
- ✅ Edit office details (name, region, leader)
- ✅ Delete office with confirmation dialog
- ✅ Office stats: leader name, user count, active projects
- ✅ API routes: `/api/admin/users`, `/api/admin/users/[userId]`, `/api/admin/users/[userId]/reset-password`, `/api/admin/offices`, `/api/admin/offices/[officeId]`
- ✅ Zod validation for all admin API inputs
- ✅ Database schema updates (offices table, last_login_at, region columns)
- ✅ RBAC enforcement (super_admin only access)
- ✅ E2E tests for user and office CRUD operations
- ✅ Unit tests for admin validation schemas

### User Management Features:
- Super admins can create, edit, and deactivate users
- Super admins can reset user passwords (generates temporary password)
- Super admins can assign users to offices and roles
- Super admins can filter users by role and office
- Super admins can search users by name or email

### Office Management Features:
- Super admins can create, edit, and delete offices
- Super admins can assign office leaders
- Super admins can view office stats (team size, active projects)
- Super admins can search offices by name or region
- Delete confirmation prevents accidental deletion

### Security & Observability Hardening
- ✅ HTTPS-only NextAuth cookies with secure, httpOnly, sameSite settings
- ✅ Environment-aware cookie configuration (secure in production, standard in dev)
- ✅ Authorization guard helpers (requireAuth, requireRole, requireProjectAccess)
- ✅ Role-based API route protection
- ✅ Lightweight logging module with console and Sentry stub
- ✅ Quickbase API request/response logging with timing
- ✅ Offline sync error capture with structured logging
- ✅ Sentry client and server configuration stubs (optional)
- ✅ Security audit documentation (SECURITY-READINESS.md)
- ✅ npm audit run and vulnerabilities documented
- ✅ Input validation and sanitization on API routes

**Security Features:**
- Authentication: Bcrypt password hashing, JWT sessions, secure cookies
- Authorization: Role-based guards, session validation
- Logging: Request/response logging, error tracking, Sentry integration (optional)
- Input Validation: Type checking, sanitization, error handling

## Testing

### Quick Test Commands

```bash
npm run test:all              # Run all tests (unit + E2E)
npm run test:unit             # Run unit tests once
npm run test:unit:watch       # Run unit tests in watch mode
npm run test:integration      # Run E2E tests
npm run test:coverage:report  # Generate and open coverage report
```

### Unit Tests (Vitest)

**Test Coverage:**
- ✅ Quickbase client (rate limiting, queue, error handling)
- ✅ Offline storage (IndexedDB, cache, mutation queue)
- ✅ Sync queue (retry logic, online/offline events)
- ✅ Milestone utilities (status calculation, progress, urgency)
- ✅ Milestone status (detailed substeps, warnings, SLA)
- ✅ Traffic lights (7 milestone state calculators)
- ✅ Formatters (date, currency, system size, PPW)
- ✅ Hold detection (status detection, type extraction)
- ✅ Project helpers (name parsing, address formatting)

**Coverage Target:** >80% for `lib/` directory

**Run unit tests:**
```bash
npm run test:unit          # Run once (for CI)
npm run test:unit:watch    # Watch mode (for development)
npm run test:coverage      # With coverage report
```

**View coverage report:**
```bash
npm run test:coverage:report
# Opens tests/coverage/index.html in browser
```

### Integration Tests (Playwright)

**Test Coverage:**
- ✅ Complete user journey (login → dashboard → projects → detail → hold management)
- ✅ Dashboard metrics and navigation
- ✅ Projects list (search, filters, traffic lights, hold banners)
- ✅ Offline scenarios (indicator, mutation queuing, sync, cache fallback)
- ✅ Hold management (place, update, release)

**Browsers:** Chromium (Desktop 1280×720), iPad Pro (1024×768)

**Run integration tests:**
```bash
npm run test:integration       # Run all E2E tests
npm run test:integration:ui    # Run with Playwright UI
npm run test:e2e:headed        # Run with visible browser
npm run test:e2e:debug         # Run in debug mode
npm run test:offline           # Run offline tests only
```

**View test report:**
```bash
npx playwright show-report
# Opens playwright-report/index.html in browser
```

### CI Integration

**Run all tests for CI:**
```bash
npm run test:ci
```

This command:
1. Runs all unit tests with coverage
2. Runs all E2E tests on chromium + iPad
3. Generates coverage reports (HTML + LCOV)
4. Exits with code 1 if any test fails

**Coverage artifacts:**
- `tests/coverage/index.html` - HTML coverage report
- `tests/coverage/lcov.info` - LCOV format for CI tools
- `playwright-report/index.html` - E2E test report

### Known Issues

See `docs/QA-HANDOFF.md` for detailed list of known issues and flaky tests.
- Edge cases: missing fields, date calculations, urgency thresholds
- Target: >90% coverage on utility functions

### Integration Tests

Run integration tests for offline functionality:

```bash
npm run test:integration        # Run all integration tests
npm run test:integration:ui     # Run with Playwright UI
npm run test:offline            # Run offline tests only
```

**Test coverage:**
- Offline/online transitions
- Mutation queuing and sync
- Cache fallback behavior
- Service worker caching strategies
- PWA installation flow

## PWA Installation

### iPad Installation
1. Open the app in Safari
2. Tap the Share button
3. Tap 'Add to Home Screen'
4. Tap 'Add'
5. App icon appears on home screen

### Android Installation
1. Open the app in Chrome
2. Tap the menu (three dots)
3. Tap 'Install app' or 'Add to Home Screen'
4. Tap 'Install'
5. App icon appears on home screen

### Desktop Installation (Chrome)
1. Open the app in Chrome
2. Click the install icon in the address bar
3. Click 'Install'
4. App opens in standalone window

## Offline Usage

The app works offline with the following capabilities:

**Available offline:**
- View cached project list (last 5 minutes)
- View cached project details
- View project timeline and milestones
- Place projects on hold (queued for sync)
- Release projects from hold (queued for sync)
- Update hold reasons (queued for sync)

**Requires online:**
- Fetching new projects
- Updating project details (non-hold fields)
- Viewing real-time data
- Syncing queued mutations

**Sync behavior:**
- Queued mutations sync automatically when connectivity returns
- Failed syncs retry up to 3 times
- Sync status shown in offline indicator
- Manual sync available via refresh

## Deployment & Launch Readiness

### Deployment & Launch Readiness
- ✅ Vercel configuration finalized (vercel.json)
- ✅ Production smoke test suite created
- ✅ Deployment runbook documented (DEPLOYMENT-RUNBOOK.md)
- ✅ Token rotation process documented (TOKEN-ROTATION.md)
- ✅ User onboarding checklist created (USER-ONBOARDING.md)
- ✅ Support runbook with escalation paths (SUPPORT-RUNBOOK.md)
- ✅ Success metrics dashboard outlined (SUCCESS-METRICS.md)
- ✅ PWA installation verified on iPad
- ✅ Launch notes documented (LAUNCH-NOTES.md)
- ✅ Production environment variables configured
- ✅ Custom domain setup documented
- ✅ Monitoring and alerting configured

**Deployment Features:**
- Vercel hosting with auto-deploy from main branch
- Preview deployments for all branches
- Environment-specific configurations (Production/Preview/Dev)
- Zero-downtime deployments with instant rollback
- Production smoke tests for post-deployment verification

## Audit Logs & Compliance

### Settings: Audit Logs & Compliance (Super Admin)
- ✅ AuditLogsTab component with filters, pagination, and detail view
- ✅ Date range picker for filtering logs by timeframe
- ✅ Action type filter (login, logout, create, update, delete, export)
- ✅ Search by user name, resource type, or IP address
- ✅ Paginated table view (50 logs per page)
- ✅ Detail dialog showing full audit log with JSON diffs
- ✅ JSON diff utility showing old → new values in red/green
- ✅ CSV export functionality for compliance reporting
- ✅ API routes: `/api/admin/audit-logs` (GET with pagination), `/api/admin/audit-logs/export` (GET with CSV)
- ✅ Database schema update (audit_logs table with indexes)
- ✅ Audit logging integrated into all admin API routes
- ✅ Profile, password, user, office, and system settings changes tracked
- ✅ RBAC enforcement (super_admin only access)
- ✅ E2E tests for audit log filtering, pagination, detail view, CSV export
- ✅ Unit tests for JSON diff utilities
- ✅ Compliance monitoring documentation

**Audit Logging Features:**
- All administrative actions automatically logged
- Immutable audit trail (logs cannot be edited/deleted)
- JSON diffs show exactly what changed
- User attribution tracks who made each change
- IP address and user agent tracking
- CSV export for external analysis and compliance reporting
- Server-side pagination for performance with large datasets

## Settings: Profile & Notifications

### Settings: Profile & Notifications
- ✅ Settings page with tabbed interface (`/settings`)
- ✅ ProfileTab component with profile editing (name, email, phone)
- ✅ Password change functionality with validation
- ✅ Danger zone (sign out all sessions)
- ✅ NotificationsTab component with email toggles
- ✅ Alert thresholds for office leaders and above
- ✅ Test notification button
- ✅ API routes: `/api/user/profile`, `/api/user/password`, `/api/user/notifications`, `/api/user/test-notification`
- ✅ Zod validation for all API inputs
- ✅ Database schema updates (phone field, notification_settings table)
- ✅ Role-based tab visibility (placeholder tabs for future phases)
- ✅ TanStack Query mutations with optimistic updates
- ✅ Toast notifications for all actions
- ✅ E2E tests for settings functionality
- ✅ Unit tests for validation schemas

**Settings Features:**
- All users can update their profile (name, email, phone)
- All users can change their password (with current password verification)
- All users can configure email notification preferences
- Office leaders+ can set custom alert thresholds
- Test notification button to verify email settings
- Sign out all sessions for security

## Deployment

### Quick Deploy

**Deploy to Vercel:**
```bash
# Deploy to preview environment
npm run deploy:preview

# Deploy to production
npm run deploy:production
```

All CI checks must pass before production deployment. Preview deployments are posted to PRs automatically. See `docs/CI-CD-SETUP.md`.

**Or use Git:**
```bash
# Push to main branch (auto-deploys to production)
git push origin main

# Push to any other branch (auto-deploys to preview)
git push origin feature-branch
```

### Production Smoke Tests

**After deployment, run smoke tests:**
```bash
BASE_URL=https://your-app.vercel.app npm run test:smoke
```

This verifies:
- ✅ Site is accessible
- ✅ Login works
- ✅ Dashboard loads with real data
- ✅ Projects list works
- ✅ PWA features work
- ✅ Performance meets targets

### Deployment Documentation

- **Deployment Runbook:** `docs/DEPLOYMENT-RUNBOOK.md`
- **Token Rotation:** `docs/TOKEN-ROTATION.md`
- **Support Guide:** `docs/SUPPORT-RUNBOOK.md`
- **Launch Notes:** `docs/LAUNCH-NOTES.md`

### Rollback

If deployment has issues:

1. Go to Vercel → Deployments
2. Find last known good deployment
3. Click ⋯ → "Promote to Production"
4. Deployment rolls back in ~30 seconds

See `docs/DEPLOYMENT-RUNBOOK.md` Step 7 for detailed rollback procedures.

## User Onboarding

**For Sales Reps:**

See `docs/USER-ONBOARDING.md` for complete onboarding guide.

**Quick Start:**
1. Install app on iPad (Safari → Share → Add to Home Screen)
2. Login with Kin Home email and password
3. View dashboard to see your metrics
4. Tap "Projects" to see your pipeline
5. Use search and filters to find projects
6. Tap any project to see details and timeline
7. Manage holds from project detail page

**Training Checklist:**
- [ ] Account created
- [ ] App installed on iPad
- [ ] Completed first login
- [ ] Viewed dashboard
- [ ] Searched for project
- [ ] Placed/released hold
- [ ] Tested offline functionality

See `docs/USER-ONBOARDING.md` for detailed training checklist.

## Settings

### User Profile

All users can manage their profile and preferences:

**Profile Management:**
- Update name, email, and phone number
- View Quickbase user ID and office assignment
- Change password (requires current password)
- Sign out from all devices

**Notification Preferences:**
- Enable/disable email notifications
- Configure urgent alerts, daily digest, weekly summary
- Set custom alert thresholds (office leaders and above)
- Send test email to verify settings

**Access Settings:**
1. Click "Settings" in the sidebar
2. Use tabs to navigate between Profile and Notifications
3. Make changes and click Save
4. Changes sync immediately

### Audit Logs (Super Admin Only)

**Audit Logs Tab:**
- View all system activity and user actions
- Filter by date range (default: last 30 days)
- Filter by action type (create, update, delete, etc.)
- Search by user name, resource type, or IP address
- Paginated view (50 logs per page)
- Click "Details" to see full JSON diff of changes
- Export filtered logs to CSV for compliance reporting

**What's Logged:**
- User profile updates (name, email, phone)
- Password changes (user and admin-initiated)
- User management (create, update, deactivate users)
- Office management (create, update, delete offices)
- System settings changes (Quickbase config, SLA defaults)
- Export events (CSV downloads)

**Compliance Use Cases:**
- Security audits (review all deletions, user changes)
- Incident investigation (filter by date range and resource)
- Change history (track system configuration changes)
- Regulatory compliance (SOC 2, GDPR, internal policies)

**Access:** Only super_admin role can access audit logs.

## Project Status

See `docs/IMPLEMENTATION-STATUS.md` for the detailed checklist with current completion levels and timelines. This app is nearing production; CI/CD pipeline is in place and manual QA is underway.

## Production Readiness

Current status: ~85% ready. CI/CD added; manual QA pending. See `docs/PRODUCTION-READINESS.md` for a full assessment.

**Total Implementation:**
- 13 phases completed
- 150+ unit tests
- 30+ E2E tests
- >80% code coverage
- Full offline support
- Comprehensive admin features
- Compliance-ready audit logging

### User Management (Super Admin Only)

**Users Tab:**
- View all users in a searchable table
- Filter by role (closer, setter, office leader, regional, super admin)
- Filter by office assignment
- Add new users with role and office assignment
- Toggle user active/inactive status
- Reset user passwords (generates temporary password)
- View last login timestamp

**Offices Tab:**
- View all offices in a grid with stats
- Create new offices with region and leader assignment
- Edit office details (name, region, leader)
- Delete offices (with confirmation)
- View office stats: leader, team size, active projects
- Search offices by name or region

**Access:** Only super_admin role can access these tabs.

## Performance Targets

- Initial load: <2 seconds
- Dashboard refresh: <1 second
- Project detail: <1.5 seconds
- Search results: <500ms
- All API calls: <1 second (95th percentile)

## Success Criteria

- Load time <2 seconds ✓
- Find project status in <3 seconds ✓
- Complete task in ≤2 taps ✓
- 90%+ daily active users (after launch)
- 30%+ increase in task completion rate
- 50% reduction in hold resolution time

## Success Metrics

**Track post-launch:**
- Daily Active Users (target: 90%+)
- PWA Installation Rate (target: 80%+)
- Task Completion Rate (target: 30%+ increase)
- Hold Resolution Time (target: 50% reduction)
- Page Load Times (target: <2 seconds)
- User Satisfaction (NPS target: >50)

See `docs/SUCCESS-METRICS.md` for detailed metrics dashboard.
