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
- **PDF Export**: jsPDF + html2canvas for component export functionality

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

## Developer Resources

### 📚 Essential Documentation for Developers

**New to the codebase? Start here:**

1. **[Developer Guide](docs/DEVELOPER-GUIDE.md)** - Coding patterns, best practices, and common pitfalls
2. **[Database Schema Reference](docs/DATABASE-SCHEMA.md)** - Complete database schema, SQL patterns, and query examples
3. **[Setup Guide](SETUP.md)** - Initial development environment setup

**Key topics:**
- **Database queries**: Always check `sales_office` is an array, not a string! See [DATABASE-SCHEMA.md](docs/DATABASE-SCHEMA.md#users-table)
- **SQL patterns**: Common query patterns for users, offices, and filtering - [DATABASE-SCHEMA.md](docs/DATABASE-SCHEMA.md#common-query-patterns)
- **API development**: Authentication, validation, logging patterns - [DEVELOPER-GUIDE.md](docs/DEVELOPER-GUIDE.md#api-development)
- **Testing requirements**: What to test before submitting PRs - [DEVELOPER-GUIDE.md](docs/DEVELOPER-GUIDE.md#testing-requirements)
- **Database migrations**: Located in `lib/db/migrations/` - check these FIRST before writing SQL

**Quick Reference:**
- Database schema definitions: `lib/db/migrations/*.sql`
- Example SQL queries: Search codebase with `grep -r "FROM users" app/api`
- API route patterns: See existing routes in `app/api/`
- Component patterns: See existing components in `components/`

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

## Baseball Card Component

The Baseball Card component (`components/rep/BaseballCard.tsx`) displays a comprehensive profile of a sales rep's performance by combining RepCard canvassing data with QuickBase sales data.

### Features

- **Volume Stats**: Doors knocked, appointments set, sales closed, revenue generated
- **Quality Metrics**: Appointment speed, power bill attachment rate, reschedule rate, follow-up consistency
- **Efficiency Metrics**: Doors per appointment, appointments per sale, average deal size, time to close
- **Leaderboard Rankings**: Overall rank and office rank with trend indicators
- **PDF Export**: Export the baseball card as a PDF for sharing or printing
- **Graceful Degradation**: Shows appropriate message when RepCard data is unavailable
- **Responsive Design**: Mobile-friendly layout that adapts to screen size

### Usage

```tsx
import { BaseballCard } from '@/components/rep';

<BaseballCard
  userId="user-id-123"
  startDate="2024-01-01"
  endDate="2024-01-31"
  timeRange="January 2024"
  showExport={true}
/>
```

### Props

- `userId` (required): Dashboard user ID
- `startDate` (optional): Start date for metrics (YYYY-MM-DD), defaults to current month start
- `endDate` (optional): End date for metrics (YYYY-MM-DD), defaults to today
- `timeRange` (optional): Human-readable time range label for display
- `showExport` (optional): Whether to show PDF export button (default: true)
- `className` (optional): Additional CSS classes

### Data Sources

- **RepCard API**: Canvassing data (doors knocked, appointments, quality metrics)
- **Stats API**: Sales data (closed deals, revenue) sourced indirectly from QuickBase via the stats endpoint
- **Leaderboard API**: Rankings and trends

### PDF Export

The component uses `html2canvas` and `jsPDF` to capture the card and generate a PDF. The export functionality:

1. Captures the card element as a high-resolution canvas
2. Converts the canvas to a PDF document
3. Downloads the PDF with filename format: `{rep-name}-baseball-card.pdf`

### Caching

- User stats: 15-minute cache
- Leaderboard data: 15-minute cache
- All data is fetched via React Query with automatic refetching on stale data

### Error Handling

- Shows loading skeleton while fetching data
- Displays error message with retry option on API failures
- Shows "Not linked to RepCard" message when RepCard ID is missing
- Handles missing or incomplete data gracefully

### Integration

The Baseball Card is currently integrated into:
- Rep detail page (`/analytics/rep/[id]`)

It can be added to any page that needs to display comprehensive rep profiles.

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

### User Sync Scripts

**Purpose:** Daily refresh of user data from QuickBase Contacts table

The `sync-users-from-contacts.ts` script refreshes all existing users from the QuickBase Contacts table using the existing `enrichUserFromContacts()` function. This is different from the seed script which creates NEW users from projects.

**Available Commands:**
```bash
npm run sync:contacts                    # Full sync of all users
npm run sync:contacts:dry-run           # Preview changes without updating
npm run sync:contacts -- --limit=10     # Test with first 10 users
npm run sync:contacts -- --force        # Force sync even if recently synced
npm run sync:contacts -- --verbose      # Detailed logging
```

**When to run:**
- Daily via cron job (automated)
- After bulk updates to QuickBase Contacts table
- When RepCard IDs or Enerflo IDs are updated in Contacts
- When office/team assignments change

**What it does:**
- Queries all users from local database
- For each user, looks up in QuickBase Contacts table by email
- Updates missing fields using COALESCE strategy (never overwrites existing data)
- Logs all operations to user_sync_log table
- Provides detailed summary report

**Troubleshooting:**
- If many users not found in Contacts: Check email addresses match between systems
- If high error rate: Check QuickBase API credentials and rate limits
- If sync is slow: Use --limit flag to test with smaller batches first

Reference the existing enrichment infrastructure in `lib/users/enrich-user.ts` and field mappings in `lib/constants/contactFieldIds.ts`.

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

## Components

### ConfigurableLeaderboard Component

The ConfigurableLeaderboard component (`components/analytics/ConfigurableLeaderboard.tsx`) displays a ranked list of sales reps based on various performance metrics from RepCard and QuickBase data.

#### Features

- **Flexible Filtering**: Role (Setter/Closer/All), Metric (8 options), Time Period (7 presets + custom), Office (multi-select)
- **Visual Rankings**: Top 3 performers get medal badges (🥇🥈🥉) with gradient backgrounds
- **Trend Indicators**: Shows performance trends (↑↓→⭐) with color coding
- **Real-time Updates**: Auto-refreshes every 15 minutes via React Query
- **Export to CSV**: Download leaderboard data with formatted values
- **Click to Navigate**: Click any entry to view rep detail page
- **Responsive Design**: Mobile-friendly with horizontal scroll for tables
- **Collapsible**: Optional collapsible card for space-saving layouts
- **Customizable**: Accepts props for default filters and preset views

#### Usage

**Basic Usage:**
```tsx
import { ConfigurableLeaderboard } from '@/components/analytics';

<ConfigurableLeaderboard />
```

**Preset View (Top Setters by Doors Knocked):**
```tsx
<ConfigurableLeaderboard
  defaultRole="setter"
  defaultMetric="doors_knocked"
  defaultTimeRange="month"
  title="Top Setters - Doors Knocked"
  description="Leading setters by door knocking activity this month"
  limit={25}
/>
```

**Preset View (Top Closers by Revenue):**
```tsx
<ConfigurableLeaderboard
  defaultRole="closer"
  defaultMetric="revenue"
  defaultTimeRange="quarter"
  title="Top Closers - Revenue"
  description="Highest revenue generators this quarter"
  limit={25}
/>
```

**Quality Leaders (All Roles):**
```tsx
<ConfigurableLeaderboard
  defaultRole="all"
  defaultMetric="quality_score"
  defaultTimeRange="month"
  title="Quality Leaders"
  description="Reps with highest quality scores this month"
  limit={50}
/>
```

**Collapsible with Hidden Filters:**
```tsx
<ConfigurableLeaderboard
  collapsible={true}
  defaultOpen={false}
  showFilters={false}
  defaultMetric="sales_closed"
  title="Sales Leaderboard"
/>
```

#### Props

- `defaultRole?: 'setter' | 'closer' | 'all'` - Default role filter (default: 'all')
- `defaultMetric?: LeaderboardMetric` - Default metric (default: 'quality_score')
- `defaultTimeRange?: TimeRange` - Default time range (default: 'month')
- `defaultOfficeIds?: number[]` - Default office IDs to filter by
- `showFilters?: boolean` - Show filter controls (default: true)
- `showExport?: boolean` - Show export button (default: true)
- `showRefresh?: boolean` - Show manual refresh button (default: true)
- `limit?: number` - Number of entries to display (default: 50)
- `title?: string` - Custom card title (default: 'Leaderboard')
- `description?: string` - Custom description text
- `collapsible?: boolean` - Make card collapsible (default: false)
- `defaultOpen?: boolean` - Default collapsed state (default: true)
- `className?: string` - Additional CSS classes
- `onEntryClick?: (entry: LeaderboardEntry) => void` - Custom click handler

#### Available Metrics

- **doors_knocked**: Total doors knocked (Setter metric)
- **appointments_set**: Total appointments set (Setter metric)
- **sales_closed**: Total sales closed (Closer metric)
- **revenue**: Total revenue generated (Closer metric)
- **quality_score**: Composite quality score (Both roles)
- **appointment_speed**: % of appointments scheduled within 24 hours (Setter quality)
- **attachment_rate**: % of customers with power bill attachments (Setter quality)

#### Time Range Options

- **today**: Today's data
- **week**: This week (Monday to today)
- **month**: This month (1st to today)
- **quarter**: This quarter (Q1/Q2/Q3/Q4 start to today)
- **year**: This year (Jan 1 to today)
- **custom**: Custom date range (via date picker)

#### Data Source

- **API Endpoint**: `/api/repcard/leaderboard`
- **Cache Strategy**: 15-minute cache with auto-refresh
- **Data Combines**: RepCard canvassing data + QuickBase sales data

#### Visual Design

**Rank Badges:**
- 🥇 Rank 1: Gold medal with yellow gradient background
- 🥈 Rank 2: Silver medal with gray gradient background
- 🥉 Rank 3: Bronze medal with orange gradient background
- Rank 4+: Numbered badge with blue background

**Trend Indicators:**
- ↑ (Green): Performance improved vs previous period
- ↓ (Red): Performance declined vs previous period
- → (Gray): Performance unchanged
- ⭐ (Blue): New entry (first time on leaderboard)

#### Integration

The ConfigurableLeaderboard is designed to be integrated into the Analytics page with preset views:
- **Leaderboards Tab**: Multiple preset leaderboards (Top Setters, Top Closers, Quality Leaders, Volume Leaders)
- **Dashboard**: Featured leaderboard widget on main dashboard
- **Office Pages**: Office-specific leaderboards

See the next phase documentation for Analytics page integration details.

#### Error Handling

- Shows loading skeleton while fetching data
- Displays error message with retry button on API failures
- Shows empty state when no data matches filters
- Handles missing RepCard data gracefully
- Validates date ranges before API calls

#### Performance

- React Query caching reduces API calls
- 15-minute auto-refresh keeps data fresh
- Memoized calculations for formatting and badges
- Efficient re-renders with proper React keys
- Lazy loading of office data

#### Accessibility

- ARIA labels on all interactive elements
- Keyboard navigation support
- Screen reader friendly
- WCAG AA color contrast compliance
- Tooltips for metric explanations

---

**Note:** This component is part of the RepCard integration phase. It will be consumed by the Analytics page in the next implementation phase.

## Setter and Closer Performance Tables

Role-specific performance tables that combine RepCard canvassing data with QuickBase sales data to provide comprehensive insights into setter and closer performance.

### SetterPerformanceTable

**Location:** `components/analytics/SetterPerformanceTable.tsx`

**Purpose:** Display setter-specific metrics including canvassing activity, appointment setting, and quality indicators.

**Columns:**
- **Rank**: Position based on selected sort column
- **Setter Name**: Clickable link to rep detail page
- **Office**: Office assignment
- **Doors Knocked**: Total doors knocked (from RepCard)
- **Appointments Set**: Total appointments set (from RepCard)
- **Set Rate (%)**: Conversion rate from doors to appointments (calculated: appointments / doors * 100)
- **Show Rate (%)**: Percentage of appointments where customer showed up (from appointment_speed metric)
- **Close Rate (%)**: Percentage of appointments that resulted in sales (calculated: sales / appointments * 100)
- **Quality Score**: Composite quality score (from quality metrics service)

**Data Sources:**
- RepCard API: `/api/repcard/leaderboard` with multiple metric queries (doors_knocked, appointments_set, quality_score, appointment_speed, sales_closed)
- QuickBase: Sales closed data for close rate calculation

**Features:**
- Sortable columns with visual indicators
- Search by setter name
- Color-coded metrics (green/yellow/red based on thresholds)
- Export to CSV
- Collapsible card with localStorage persistence
- Responsive design with horizontal scroll

**Usage:**
```typescript
import { SetterPerformanceTable } from '@/components/analytics';

<SetterPerformanceTable
  userId={session.user.id}
  role={session.user.role}
  timeRange="last_12_months"
  officeIds={[1, 2, 3]}
  showExport={true}
/>
```

### CloserPerformanceTable

**Location:** `components/analytics/CloserPerformanceTable.tsx`

**Purpose:** Display closer-specific metrics including appointment conversion, revenue generation, and follow-up activity.

**Columns:**
- **Rank**: Position based on selected sort column
- **Closer Name**: Clickable link to rep detail page
- **Office**: Office assignment
- **Appointments Sat**: Total appointments attended (from QuickBase projects count)
- **Sales Closed**: Total deals closed (from QuickBase)
- **Sit/Close (%)**: Conversion rate from appointments to sales (calculated: sales / appointments * 100)
- **Revenue**: Total revenue generated (from QuickBase system_cost sum)
- **Avg Deal Size**: Average revenue per sale (calculated: revenue / sales)
- **Follow-Ups**: Total follow-up appointments completed (from quality metrics)

**Data Sources:**
- RepCard API: `/api/repcard/leaderboard` for sales_closed and revenue metrics
- QuickBase: Direct database queries for appointments sat (project count per closer)
- Quality Metrics API: `/api/repcard/users/[userId]/quality-metrics` for follow-up data

**Features:**
- Sortable columns with visual indicators
- Search by closer name
- Color-coded sit/close rate (green ≥30%, yellow 20-30%, red <20%)
- Export to CSV
- Collapsible card with localStorage persistence
- Responsive design with horizontal scroll

**Usage:**
```typescript
import { CloserPerformanceTable } from '@/components/analytics';

<CloserPerformanceTable
  userId={session.user.id}
  role={session.user.role}
  timeRange="last_12_months"
  officeIds={[1, 2, 3]}
  showExport={true}
/>
```

### Helper Module: closerMetrics.ts

**Location:** `lib/analytics/closerMetrics.ts`

**Purpose:** Centralize complex closer metric queries that require custom database logic.

**Exports:**
- `getAppointmentsSatByCloser()`: Fetch appointment counts per closer from QuickBase projects
- `getFollowUpsByCloser()`: Fetch follow-up counts per closer from quality metrics
- `calculateDateRange()`: Helper to convert TimeRange to date strings

**Usage:**
```typescript
import { getAppointmentsSatByCloser, getFollowUpsByCloser } from '@/lib/analytics/closerMetrics';

const appointmentsSat = await getAppointmentsSatByCloser('last_12_months', undefined, [1, 2, 3]);
const followUps = await getFollowUpsByCloser('last_12_months', undefined, [1, 2, 3]);
```

### Integration

Both tables are integrated into the **Analytics Performance tab** alongside the existing RepPerformanceTable:

1. **RepPerformanceTable**: General performance for all reps
2. **SetterPerformanceTable**: Setter-specific metrics
3. **CloserPerformanceTable**: Closer-specific metrics
4. **RepBenchmarkComparisonCard**: Benchmark comparisons
5. **MilestonePerformanceCard**: Milestone tracking

All tables respect page-level filters (time range, office selection) and are collapsible for better space management.

### Performance Considerations

**SetterPerformanceTable:**
- Makes 5 parallel API calls to leaderboard endpoint
- Uses React Query caching (15-minute stale time)
- Combines data client-side for flexibility

**CloserPerformanceTable:**
- Makes 2 leaderboard API calls + custom database queries
- Follow-ups query is expensive (N API calls per closer)
- Consider implementing batch endpoint for follow-ups if performance becomes an issue
- Uses aggressive caching (30 minutes) for quality metrics

### Color Coding Thresholds

**Setter Metrics:**
- Set Rate: Green ≥20%, Yellow 10-20%, Red <10%
- Show Rate: Green ≥80%, Yellow 60-80%, Red <60%
- Close Rate: Green ≥30%, Yellow 20-30%, Red <20%
- Quality Score: Green ≥80%, Yellow 60-80%, Red <60%

**Closer Metrics:**
- Sit/Close %: Green ≥30%, Yellow 20-30%, Red <20%

### Future Enhancements

- Add trend indicators (↑↓) showing change vs previous period
- Implement batch endpoint for follow-ups to improve performance
- Add drill-down capability (click row to see detailed breakdown)
- Add comparison to office/company benchmarks
- Add goal lines and target indicators
- Add export to PDF functionality
- Add real-time updates with WebSocket or polling

---

**Related Documentation:**
- RepCard Integration: See RepCard API Endpoints section
- Quality Metrics: See RepCard Quality Metrics section
- Analytics Page: See Analytics Page - Tab Structure section

## RepCard API Endpoints

The RepCard integration provides four API endpoints for accessing canvassing and quality metrics data:

**1. User Stats - `/api/repcard/users/[userId]/stats`**
- **Method**: GET
- **Authentication**: Required (user can view own stats, managers can view team stats)
- **Query Parameters**:
  - `startDate` (optional): YYYY-MM-DD format, defaults to start of current month
  - `endDate` (optional): YYYY-MM-DD format, defaults to today
  - `timeRange` (optional): 'week', 'month', 'quarter', 'ytd', 'custom'
- **Response**: Comprehensive user statistics combining RepCard canvassing data with QuickBase sales data
- **Cache**: 15 minutes
- **Use Case**: Display on user profile pages, baseball cards, performance dashboards

**2. Quality Metrics - `/api/repcard/users/[userId]/quality-metrics`**
- **Method**: GET
- **Authentication**: Required (user can view own metrics, managers can view team metrics)
- **Query Parameters**:
  - `startDate` (optional): YYYY-MM-DD format
  - `endDate` (optional): YYYY-MM-DD format
  - `timeRange` (optional): 'week', 'month', 'quarter', 'ytd', 'custom'
  - `useCache` (optional): boolean, defaults to true
- **Response**: Detailed quality metrics breakdown (appointment speed, attachment rate, reschedule rate, follow-up consistency)
- **Cache**: 30 minutes (via qualityMetrics.ts)
- **Use Case**: Quality analysis, coaching insights, performance improvement tracking

**3. Leaderboard - `/api/repcard/leaderboard`**
- **Method**: GET
- **Authentication**: Required (accessible by all authenticated users)
- **Query Parameters**:
  - `role` (optional): 'setter', 'closer', 'all' (default: 'all')
  - `metric` (optional): 'doors_knocked', 'appointments_set', 'sales_closed', 'revenue', 'quality_score', 'appointment_speed', 'attachment_rate' (default: 'quality_score')
  - `timeRange` (optional): 'today', 'week', 'month', 'quarter', 'ytd', 'custom' (default: 'month')
  - `startDate` (optional): YYYY-MM-DD format (required if timeRange='custom')
  - `endDate` (optional): YYYY-MM-DD format (required if timeRange='custom')
  - `officeIds` (optional): comma-separated office names for filtering
  - `limit` (optional): number of entries to return (default: 50, max: 100)
  - `page` (optional): page number for pagination (default: 1)
- **Response**: Ranked list of users by selected metric with pagination
- **Cache**: 15 minutes
- **Use Case**: Leaderboard displays, competitive motivation, performance tracking

**4. Office Stats - `/api/repcard/offices/[officeId]/stats`**
- **Method**: GET
- **Authentication**: Required (office leaders and above)
- **Authorization**: Office leaders can only view their own office
- **Query Parameters**:
  - `startDate` (optional): YYYY-MM-DD format
  - `endDate` (optional): YYYY-MM-DD format
  - `timeRange` (optional): 'week', 'month', 'quarter', 'ytd', 'custom'
- **Response**: Office-level aggregate statistics with top performers
- **Cache**: 30 minutes
- **Use Case**: Office dashboards, team performance tracking, manager insights

**Graceful Degradation:**
All endpoints handle missing `repcard_user_id` gracefully by returning:
```json
{
  "hasRepcardData": false,
  "message": "User not linked to RepCard",
  "userId": "...",
  "userName": "..."
}
```

**Error Handling:**
- 400: Invalid parameters (with helpful error message)
- 401: Unauthorized (not authenticated)
- 403: Forbidden (authenticated but not authorized)
- 404: User/office not found
- 500: Internal server error (with error message and stack trace in development)

**Caching Strategy:**
- User stats: 15 minutes (balance between freshness and performance)
- Quality metrics: 30 minutes (calculations are expensive)
- Leaderboard: 15 minutes (needs to feel fresh for motivation)
- Office stats: 30 minutes (aggregate data changes less frequently)

**Performance Considerations:**
- All endpoints use in-memory caching with LRU eviction
- RepCard API calls are batched when possible
- Parallel data fetching with Promise.all
- Maximum cache size: 100 entries (50 for office stats)

**Related Documentation:**
- RepCard API Client: `lib/repcard/client.ts`
- Quality Metrics Service: `lib/repcard/qualityMetrics.ts`
- Type Definitions: `lib/repcard/types.ts`

## RepCard Quality Metrics

The quality metrics service calculates four key performance indicators from RepCard canvassing data:

**1. Appointment Speed**
- Measures how quickly appointments are scheduled after lead creation
- Target: >80% of appointments scheduled within 24 hours
- Indicates setter responsiveness and lead quality

**2. Power Bill Attachment Rate**
- Measures percentage of customers with power bill attachments
- Target: >70% of customers should have attachments
- Indicates thoroughness of lead qualification

**3. Reschedule Rate**
- Measures average number of reschedules per customer
- Target: <1.5 reschedules per customer
- Lower is better - indicates appointment quality and customer commitment

**4. Follow-Up Consistency**
- Measures percentage of customers receiving required follow-ups
- Target: >90% of customers requiring follow-ups should receive them
- Indicates closer persistence and follow-through

**Usage Examples:**

```typescript
import { getQualityMetricsForUser, getQualityMetricsForOffice } from '@/lib/repcard';

// Get metrics for a single user
const userMetrics = await getQualityMetricsForUser(
  'user-id-123',
  '2024-01-01',
  '2024-01-31'
);

// Get metrics for an entire office
const officeMetrics = await getQualityMetricsForOffice(
  'Phoenix Office',
  '2024-01-01',
  '2024-01-31'
);

// Get metrics for multiple users
const metrics = await getQualityMetricsForUsers({
  userIds: ['user-1', 'user-2'],
  startDate: '2024-01-01',
  endDate: '2024-01-31'
});
```

**Caching:**
- Metrics are cached for 30 minutes to reduce API load
- Cache can be bypassed with `useCache: false` parameter
- Cache statistics available via `getCacheStats()`
- Clear cache with `clearQualityMetricsCache()`

**Performance:**
- Typical calculation time: 2-5 seconds (depending on data volume)
- Cached responses: <10ms
- API calls are made in parallel for optimal performance

**Testing:**
- Run tests: `npm test qualityMetrics.test.ts`
- Tests cover all calculation functions and edge cases
- Mock data ensures consistent test results

**Future Enhancements:**
- Add configurable status IDs for reschedule detection
- Implement file type filtering for power bill attachments
- Refine follow-up consistency logic based on business requirements
- Add trend analysis and forecasting

## Analytics Page - Tab Structure

The Analytics page (`app/(sales)/analytics/page.tsx`) provides comprehensive performance insights with 6 tabs:

### 1. Overview Tab
- **Purpose**: High-level office and pipeline metrics
- **Components**: OfficeOverviewCard, PipelineForecastCard
- **Access**: Office leaders and above

### 2. Performance Tab
- **Purpose**: Individual rep performance analysis
- **Components**: RepPerformanceTable, SetterPerformanceTable, CloserPerformanceTable, RepBenchmarkComparisonCard, MilestonePerformanceCard
- **Access**: Office leaders and above

### 3. Comparisons Tab
- **Purpose**: Period-over-period and benchmark comparisons
- **Components**: PeriodComparisonCard, BenchmarkComparisonCard, VisualComparisonsCard, OfficeComparisonTable
- **Access**: Office leaders and above

### 4. Analysis Tab
- **Purpose**: Deep-dive analysis of cancellations and holds
- **Components**: CancellationAnalysisCard, HoldAnalysisCard (currently commented out)
- **Access**: Office leaders and above

### 5. Leaderboards Tab ⭐ NEW
- **Purpose**: Competitive rankings and performance leaderboards
- **Components**: 4 preset ConfigurableLeaderboard instances
  - **Top Setters - Doors Knocked**: Leading setters by door knocking activity (25 entries, open by default)
  - **Top Closers - Revenue**: Highest revenue generators (25 entries, collapsed by default)
  - **Quality Leaders**: Reps with highest quality scores (50 entries, collapsed by default)
  - **Volume Leaders - Appointments**: Reps with most appointments set (50 entries, collapsed by default)
- **Features**:
  - Each leaderboard has independent filters (role, metric, time range, office)
  - Export to CSV functionality
  - Real-time updates (15-minute auto-refresh)
  - Collapsible cards for space management
  - Medal badges for top 3 performers (🥇🥈🥉)
  - Trend indicators (↑↓→⭐)
- **Access**: Office leaders and above
- **Data Source**: `/api/repcard/leaderboard` with RepCard canvassing data + QuickBase sales data

### 6. Canvassing Tab ⭐ NEW
- **Purpose**: Canvassing activity analysis and lead quality metrics
- **Components**:
  - **CanvassingOverviewCard**: High-level metrics (total doors, appointments, conversion rate, active reps)
  - **DoorsKnockedTrendsCard**: Line chart showing door knocking trends over time (placeholder with mock data)
  - **AppointmentRatesCard**: Bar chart showing appointment set rates by office/rep
  - **LeadQualityAnalysisCard**: Quality metrics breakdown (show rate, sit rate, close rate, follow-up rate) (placeholder with mock data)
- **Features**:
  - Uses page-level filters (time range, office selection)
  - Responsive grid layout (side-by-side on desktop, stacked on mobile)
  - Color-coded quality indicators (green/yellow/red)
  - Target benchmarks for quality metrics
- **Access**: Office leaders and above
- **Data Source**: `/api/repcard/leaderboard` (current), future: dedicated canvassing endpoints
- **Status**: Placeholder components with basic functionality. Full implementation requires:
  - Time-series data from RepCard API for trend charts
  - Aggregated quality metrics endpoint
  - Enhanced visualizations and drill-down capabilities

### Tab Navigation
- **URL-based state**: Tab selection persists in URL (`?tab=leaderboards`)
- **Responsive design**: 3 columns on mobile (2 rows), 6 columns on desktop (1 row)
- **Smooth transitions**: Fade-in animations when switching tabs
- **Scroll behavior**: Auto-scroll to top when changing tabs

### Filter Integration
- **Page-level filters**: Time range, office selection, rep selection (managed in `page.tsx`)
- **Leaderboards tab**: Each leaderboard has independent filters that override page-level defaults
- **Other tabs**: Use page-level filters directly
- **URL persistence**: All filters persist in URL for shareable links

### Access Control
- **Required role**: Office leader, Regional, or Super Admin
- **Office scoping**: Office leaders see only their assigned offices by default
- **Regional/Super Admin**: See all offices by default

### Future Enhancements
- **Canvassing Tab**:
  - Replace mock trend data with real time-series data from RepCard API
  - Implement aggregated quality metrics endpoint
  - Add drill-down capabilities (click chart to see rep details)
  - Add comparison to previous period
  - Add goal lines and target indicators
- **Leaderboards Tab**:
  - Add more preset leaderboards (e.g., "Most Improved", "Consistency Leaders")
  - Add team leaderboards (office vs office)
  - Add historical trend view (rank changes over time)
- **General**:
  - Add export functionality for all tabs
  - Add scheduled reports (email daily/weekly summaries)
  - Add custom dashboard builder (drag-and-drop widgets)

---

**Related Documentation:**
- ConfigurableLeaderboard Component: See Components section
- RepCard Integration: See RepCard API Endpoints section
- Analytics API Routes: See API Routes section

## User Sync System

The user sync system automatically refreshes user data from QuickBase Contacts table daily, ensuring the dashboard has up-to-date RepCard IDs, Enerflo IDs, office assignments, and other user metadata.

### Overview

**Purpose**: Sync user data from QuickBase Contacts table (master source) to local database

**Frequency**: Daily at 2 AM UTC (configurable in `vercel.json`)

**Scope**: All users with email addresses (excludes test users by default)

**Data Synced**:
- RepCard User ID
- Enerflo User ID
- Office assignment
- Team assignment
- Role (setter/closer)
- Profile image URL
- Project counts (closer/setter)

### Architecture

**Components**:
1. **Sync Script**: `scripts/sync-users-from-contacts.ts` - Core sync logic
2. **Cron Endpoint**: `/api/cron/sync-users` - Triggered by Vercel Cron
3. **Admin Endpoint**: `/api/admin/sync-users` - Manual trigger for admins
4. **Monitoring Dashboard**: `/operations/sync-monitoring` - View sync history and statistics
5. **Notification System**: `lib/notifications/slack.ts` - Alert on failures
6. **Run Logger**: `lib/sync/syncRunLogger.ts` - Track sync runs in database

**Data Flow**:
```
Vercel Cron (2 AM UTC)
  ↓
/api/cron/sync-users
  ↓
runSync() from sync script
  ↓
Query all users from local DB
  ↓
For each user:
  - Query QuickBase Contacts by email
  - Update user with COALESCE strategy
  - Log to user_sync_log table
  ↓
Aggregate statistics
  ↓
Log to user_sync_runs table
  ↓
Send Slack notification (if failures)
  ↓
Return statistics
```

### Manual Sync

**Via Admin Dashboard**:
1. Navigate to `/operations/sync-monitoring`
2. Click "Run Manual Sync" button
3. View progress and results in real-time

**Via API**:
```bash
curl -X POST https://your-domain.com/api/admin/sync-users \
  -H "Authorization: Bearer YOUR_SESSION_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"force": true}'
```

**Via CLI** (local development):
```bash
# Full sync
npm run sync:contacts

# Dry run (preview changes)
npm run sync:contacts:dry-run

# Test with limited users
npm run sync:contacts -- --limit=10

# Force sync (ignore last sync timestamp)
npm run sync:contacts -- --force

# Verbose logging
npm run sync:contacts -- --verbose
```

### Monitoring

**Dashboard**: `/operations/sync-monitoring`

**Metrics Tracked**:
- Total sync runs
- Success rate (percentage)
- Average execution time
- Last successful sync timestamp
- Consecutive failures count

**History Table**:
- Status (running, success, partial, failed)
- Start/completion timestamps
- Users processed, enriched, errors
- Triggered by (cron or manual)
- Execution time
- Error details (expandable)

**Auto-refresh**: Dashboard updates every 30 seconds

### Notifications

**Slack Notifications** (optional):
- Configure `SLACK_WEBHOOK_URL` in environment variables
- Notifications sent when:
  - Error rate > 10%
  - Complete sync failure
  - Execution time > 10 minutes

**Notification Format**:
```
🚨 User Sync Failed

Status: Partial Failure
Total Users: 196
Enriched: 180
Errors: 16 (8.2%)
Execution Time: 2m 34s

Sample Errors:
• user1@example.com: QuickBase API timeout
• user2@example.com: Not found in Contacts

View Details: https://your-domain.com/operations/sync-monitoring
```

### Configuration

**Environment Variables**:
```bash
# Required
DATABASE_URL=postgresql://...
QUICKBASE_REALM=your-realm
QUICKBASE_TOKEN=your-token
CRON_SECRET=your-cron-secret

# Optional
SLACK_WEBHOOK_URL=https://hooks.slack.com/services/...
```

**Cron Schedule** (in `vercel.json`):
```json
{
  "path": "/api/cron/sync-users",
  "schedule": "0 2 * * *"  // 2 AM UTC daily
}
```

**To change schedule**:
- Modify `schedule` in `vercel.json`
- Use cron expression format: `minute hour day month dayOfWeek`
- Examples:
  - Every 6 hours: `0 */6 * * *`
  - Twice daily (2 AM, 2 PM): `0 2,14 * * *`
  - Weekly on Monday: `0 2 * * 1`

### Troubleshooting

**High Error Rate (>10%)**:
- Check QuickBase API credentials (`QUICKBASE_TOKEN`)
- Verify QuickBase API rate limits not exceeded
- Check Contacts table data quality
- Review error details in monitoring dashboard

**High Not-Found Rate (>20%)**:
- Verify email addresses match between systems
- Check Contacts table has all active users
- Review not-found samples in monitoring dashboard

**Sync Timeout (>10 minutes)**:
- Check database connection performance
- Verify QuickBase API response times
- Consider increasing `maxDuration` in `vercel.json`

**Consecutive Failures**:
- Check environment variables are set correctly
- Verify database connection is working
- Check Vercel function logs for errors
- Try manual sync to isolate issue

**Sync Not Running**:
- Verify `CRON_SECRET` is set in Vercel environment
- Check Vercel Cron logs in dashboard
- Verify cron job is enabled in `vercel.json`
- Check function deployment status

### Database Schema

**Table: `user_sync_runs`**
- Stores aggregate statistics for each sync run
- Enables monitoring and trend analysis
- Retention: Keep all records (no automatic cleanup)

**Table: `user_sync_log`**
- Stores individual user sync operations
- Tracks confidence scores and field changes
- Retention: Keep all records for audit trail

**Migration**:
```bash
npm run migrate:sync-runs
```

### Security

**Cron Authentication**:
- Vercel Cron sends `Authorization: Bearer ${CRON_SECRET}` header
- Endpoint validates header before executing sync
- Returns 401 if authentication fails

**Admin Endpoint**:
- Requires authentication via `requireAuth()`
- Only `super_admin` role can trigger manual sync
- Returns 403 if user not authorized

**Rate Limiting**:
- Prevents concurrent syncs (checks for running status)
- Returns 429 if sync already in progress

**Data Protection**:
- User emails not exposed in public logs
- Error details only visible to admins
- Slack notifications sent to private channel

### Performance

**Typical Execution Time**:
- 200 users: ~2-3 minutes
- 500 users: ~5-7 minutes
- 1000 users: ~10-15 minutes

**Optimization**:
- Users synced sequentially to avoid rate limiting
- Recently synced users skipped (within 23 hours)
- Database queries optimized with indexes
- QuickBase API calls batched where possible

**Limits**:
- Max duration: 5 minutes (configurable in `vercel.json`)
- Max memory: 1GB
- QuickBase API rate limit: 10 requests/second

### Future Enhancements

- [ ] Email notifications (in addition to Slack)
- [ ] Retry logic with exponential backoff
- [ ] Parallel processing for faster syncs
- [ ] Incremental sync (only changed users)
- [ ] Webhook notifications to external systems
- [ ] Sync scheduling UI (change schedule without code)
- [ ] Historical trend charts in monitoring dashboard
- [ ] Export sync history to CSV

---

**Related Documentation**:
- User Enrichment: See "Self-Enriching User Database" section
- QuickBase Integration: See "QuickBase API" section
- Admin Dashboard: See "Admin Features" section
- Arrivy Integration: See "Arrivy Integration" section

## Arrivy Integration

This application integrates with Arrivy for real-time field operations tracking and crew management.

### Features
- Real-time task status updates (ENROUTE, STARTED, COMPLETE)
- Field crew location tracking
- Customer-facing live tracker URLs
- Automated notifications for delays and exceptions
- Activity feed for field events
- Two-way sync between QuickBase and Arrivy

### Setup

1. **Arrivy Account**: Sign up at [arrivy.com](https://www.arrivy.com/) and obtain API credentials from your Arrivy dashboard settings.

2. **Environment Variables**: Add to `.env.local`:
   ```
   ARRIVY_AUTH_KEY=your_auth_key
   ARRIVY_AUTH_TOKEN=your_auth_token
   ARRIVY_COMPANY_NAME=your_company_name
   ARRIVY_WEBHOOK_SECRET=your_webhook_secret
   ARRIVY_BASE_URL=https://app.arrivy.com/api
   ARRIVY_RATE_LIMIT=30
   ```

3. **Database Migration**: Run the Arrivy tables migration:
   ```bash
   psql $DATABASE_URL -f lib/db/migrations/014_create_arrivy_tables.sql
   ```

4. **Webhook Configuration**: In Arrivy dashboard, configure webhook URL:
   ```
   https://your-domain.com/api/webhooks/arrivy
   ```
   Select event types: TASK_STATUS, CREW_ASSIGNED, ARRIVING, LATE, NOSHOW

5. **Sync Entities**: Create Arrivy entities for your field crew members via the API or Arrivy dashboard.

### Usage

**Field Tracking Dashboard**: Navigate to Operations → Scheduling to view real-time field operations.

**Create Task**: Sync a QuickBase project to Arrivy:
```typescript
import { syncProjectToArrivy } from '@/lib/integrations/arrivy/service';

const result = await syncProjectToArrivy(projectId, recordId, {
  customerName: 'John Doe',
  customerPhone: '+1234567890',
  customerEmail: 'john@example.com',
  customerAddress: '123 Main St',
  city: 'San Francisco',
  state: 'CA',
  zipCode: '94105',
  taskType: 'install',
  scheduledStart: new Date('2025-11-01T09:00:00'),
  scheduledEnd: new Date('2025-11-01T17:00:00'),
  coordinatorEmail: 'coordinator@example.com',
  details: 'Standard solar installation',
});

console.log('Tracker URL:', result.trackerUrl);
```

**Customer Tracker**: Share the tracker URL with customers for real-time updates on crew arrival and task progress.

### API Endpoints

- `GET /api/operations/field-tracking/dashboard` - Dashboard data with tasks, entities, events, and metrics
- `GET /api/operations/field-tracking/tasks` - List tasks with filters
- `POST /api/operations/field-tracking/tasks` - Create/sync task from QuickBase project
- `GET /api/operations/field-tracking/tasks/[id]` - Get task details with status history
- `PUT /api/operations/field-tracking/tasks/[id]` - Update task
- `DELETE /api/operations/field-tracking/tasks/[id]` - Cancel/delete task
- `GET /api/operations/field-tracking/entities` - List crew members
- `POST /api/operations/field-tracking/entities` - Create/sync entity
- `GET /api/operations/field-tracking/events` - Query field events
- `POST /api/webhooks/arrivy` - Webhook receiver for Arrivy events

### Database Tables

- `arrivy_tasks` - Tasks synced from QuickBase to Arrivy
- `arrivy_entities` - Field crew members and technicians
- `arrivy_events` - Webhook events for audit trail and activity feed
- `arrivy_task_status` - Status updates for tasks (ENROUTE, STARTED, COMPLETE, etc.)

### Webhook Events

Arrivy sends webhook notifications for:
- Task created, updated, deleted
- Status updates (ENROUTE, START, COMPLETE, EXCEPTION, etc.)
- Crew assigned/removed
- Delays and no-shows
- Customer ratings and notes

### Troubleshooting

- **Webhook not receiving events**: Check webhook URL in Arrivy dashboard and verify HTTPS is configured
- **Tasks not syncing**: Verify API credentials (`ARRIVY_AUTH_KEY` and `ARRIVY_AUTH_TOKEN`) and check logs for errors
- **Location not updating**: Ensure crew members have Arrivy mobile app installed with location permissions enabled
- **Rate limiting**: Arrivy allows 30 requests per minute. The client automatically queues and rate-limits requests.

For more details, see [Arrivy API Documentation](https://app.arrivy.com/developer-portal).

## Success Metrics

**Track post-launch:**
- Daily Active Users (target: 90%+)
- PWA Installation Rate (target: 80%+)
- Task Completion Rate (target: 30%+ increase)
- Hold Resolution Time (target: 50% reduction)
- Page Load Times (target: <2 seconds)
- User Satisfaction (NPS target: >50)

See `docs/SUCCESS-METRICS.md` for detailed metrics dashboard.
