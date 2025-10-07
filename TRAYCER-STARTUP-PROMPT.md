# Traycer Startup Prompt

Copy and paste this into Traycer to start building the Kin Home Sales Pipeline Dashboard.

---

## Initial Prompt for Traycer

```
I need you to build a Next.js 14 PWA for solar sales reps at Kin Home. This will replace their current Quickbase interface with a modern, iPad-first dashboard.

**Project Name:** Kin Home Sales Pipeline Dashboard

**Key Requirements:**
- Next.js 14 App Router + TypeScript
- iPad/Desktop-first PWA (1024×768 primary viewport)
- Offline-capable with Service Worker + IndexedDB
- Integrates with Quickbase REST API (with rate limiting: 10 req/sec)
- 9-milestone project timeline visualization
- Role-based access (closers, setters, office leaders, regional, super admin)
- TanStack Query for data fetching + caching
- Neon PostgreSQL for user auth and caching
- Tailwind CSS + shadcn/ui components

**Critical Context:**
This project tracks solar installation projects through 9 milestones:
1. Intake → 2. Survey → 3. Design → 4. NEM → 5. Permit → 6. Install → 7. Verification → 8. Inspection → 9. PTO

**IMPORTANT DATA INSIGHT:**
- Field usage percentages DO NOT indicate reliability
- PTO has 20% usage because only 20% of projects are COMPLETE (it's the final milestone)
- HOA has 10% usage because only 10% of homes NEED HOA approval
- Include ALL milestone fields ≥10% usage (42 fields total for standard dashboard)

**Implementation Approach:**
Follow the complete implementation guide in `docs/TRAYCER-IMPLEMENTATION-BRIEF-ULTIMATE.md`. This contains:
- Phase 1: Foundation (API client with rate limiting, auth, database schema)
- Phase 2: Dashboard & Project List
- Phase 3: Project Detail with Timeline Visualization
- Phase 4: Offline Support, Testing, Deployment

**Start with Phase 1, Week 1:**
1. Set up Next.js 14 project with TypeScript
2. Implement Quickbase API client with rate limiting (10 req/sec, request queuing)
3. Set up NextAuth with Neon PostgreSQL
4. Create database schema with migrations
5. Define all 92 field constants from `data/quickbase-config.json`

Please read the following files in order:
1. `docs/TRAYCER-IMPLEMENTATION-BRIEF-ULTIMATE.md` (main implementation guide)
2. `docs/UNDERSTANDING-FIELD-USAGE.md` (critical context on field usage percentages)
3. `data/quickbase-config.json` (field ID mappings)
4. `docs/FIELD-SELECTION-STRATEGY.md` (which fields to include)

After reading these files, confirm your understanding and let's start with Phase 1: Foundation setup.
```

---

## What to Do Next

### Step 1: Open Traycer
Open Traycer in your project directory (`/Users/austinelkins/Rep Dashboard`)

### Step 2: Copy the Prompt Above
Copy the entire prompt text between the triple backticks (```).

### Step 3: Paste into Traycer
Paste it into Traycer's chat interface.

### Step 4: Let Traycer Read the Docs
Traycer will read the documentation files and confirm understanding.

### Step 5: Begin Phase 1
Once Traycer confirms, it will start implementing:
- Next.js 14 project setup
- Rate-limited Quickbase API client
- NextAuth + Neon database
- Field constants and types

---

## Expected First Response from Traycer

Traycer should respond with something like:

> "I've read the implementation guide and understand the project structure. Key insights:
> - 92 Quickbase fields to integrate
> - Rate limiting at 10 req/sec with request queuing
> - 9-milestone timeline is the core feature
> - Low usage % fields (10-20%) are critical, not unreliable
> - Starting with Phase 1: Foundation
>
> I'll begin by setting up the Next.js 14 project. Should I create the project structure now?"

---

## Files Traycer Will Reference

Traycer has access to these files in your project:

**Primary Implementation Guide:**
- `/docs/TRAYCER-IMPLEMENTATION-BRIEF-ULTIMATE.md` (3000+ lines, complete implementation)

**Critical Context:**
- `/docs/UNDERSTANDING-FIELD-USAGE.md` (why 10-20% usage fields are essential)
- `/docs/FIELD-SELECTION-STRATEGY.md` (42-field standard, 65-field complete)

**Data Mappings:**
- `/data/quickbase-config.json` (all field IDs)

**Reference Documentation:**
- `/docs/COMPLETE-FIELD-MAPPING.md` (all 92 fields documented)
- `/docs/QUICKBASE-API-REFERENCE.md` (API syntax examples)
- `/docs/ANALYSIS-SUMMARY.md` (data insights)
- `/docs/ADDERS-ANALYSIS.md` (adder system)

**Business Requirements (optional):**
- `/final-requirements-doc.md` (stakeholder-facing features)

---

## Environment Variables to Set

Before Traycer starts coding, make sure you have `.env.local` ready:

```env
# Quickbase API
QUICKBASE_REALM=kin.quickbase.com
QUICKBASE_TOKEN=YOUR_NEW_TOKEN_HERE  # ⚠️ Generate fresh token!
QUICKBASE_APP_ID=br9kwm8bk
QUICKBASE_TABLE_PROJECTS=br9kwm8na
QUICKBASE_TABLE_ADDERS=bsaycczmf
QUICKBASE_TABLE_TASKS=br9kwm8q9

# Neon Database (from your .env.local or use new project)
DATABASE_URL=postgresql://neondb_owner:npg_Fz3GX7tOdheE@ep-raspy-leaf-a55meed-pooler.us-west-2.aws.neon.tech/neondb?sslmode=require

# Auth
NEXTAUTH_SECRET=YOUR_SECRET_HERE  # Generate: openssl rand -base64 32
NEXTAUTH_URL=http://localhost:3000

# Monitoring (Optional - can add later)
NEXT_PUBLIC_SENTRY_DSN=
NEXT_PUBLIC_GA_ID=
```

**⚠️ CRITICAL:** Generate a fresh Quickbase user token:
1. Go to https://kin.quickbase.com
2. Click your profile → "My Preferences"
3. Navigate to "My User Token"
4. Generate new token
5. Copy to `.env.local`

---

## Expected Phase 1 Output (Week 1)

After ~30-60 minutes, Traycer should have created:

```
Rep Dashboard/
├── app/
│   ├── (auth)/
│   │   ├── login/page.tsx
│   │   └── register/page.tsx
│   ├── (dashboard)/
│   │   └── layout.tsx
│   ├── api/
│   │   ├── auth/[...nextauth]/route.ts
│   │   └── quickbase/
│   │       └── test/route.ts
│   └── layout.tsx
├── lib/
│   ├── quickbase/
│   │   ├── client.ts          # Rate-limited API client
│   │   ├── fields.ts          # All 92 field constants
│   │   └── types.ts           # TypeScript types
│   ├── db/
│   │   ├── schema.ts          # Neon database schema
│   │   └── client.ts
│   └── auth.ts                # NextAuth config
├── components/
│   └── ui/                    # shadcn/ui components
├── .env.local
├── package.json
└── next.config.js
```

---

## Troubleshooting

### If Traycer asks clarifying questions:
- Refer back to `docs/TRAYCER-IMPLEMENTATION-BRIEF-ULTIMATE.md`
- The implementation guide has complete code examples for everything

### If Traycer seems confused about field usage:
- Share `docs/UNDERSTANDING-FIELD-USAGE.md`
- Emphasize: "20% usage for PTO is NORMAL - it's the final milestone"

### If Traycer asks about database choice:
- Use Neon PostgreSQL (already configured)
- Connection string is in your `.env.local`

### If Traycer asks about API rate limits:
- 10 requests per second
- Implement request queuing (code is in the Ultimate Brief)

---

## Let's Go! 🚀

**Copy the prompt above and paste it into Traycer to begin!**
