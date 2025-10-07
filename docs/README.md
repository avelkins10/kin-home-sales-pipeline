# Documentation Index - Rep Dashboard

**Project**: Kin Home Rep Dashboard
**Stack**: Next.js 14, TypeScript, Quickbase, TanStack Query, shadcn/ui
**Last Updated**: 2025-01-02

---

## 📚 Quick Navigation

### Start Here
- **[IMPLEMENTATION-STATUS.md](./IMPLEMENTATION-STATUS.md)** ⭐ **START HERE**
  - What's built vs what's spec'd
  - Progress tracking (35% complete)
  - Recommended build order
  - Time estimates for each phase

### Implementation Guides
- **[PHASE-ALIGNMENT.md](./PHASE-ALIGNMENT.md)** - Dashboard & Projects List
  - Phase 4A: Dashboard page (2-3 hrs)
  - Phase 4B: Projects List page (3-4 hrs)
  - 7 vs 9 milestone explanation
  - Copy-paste prompts for Traycer/Cursor

- **[SETTINGS-SPEC.md](./SETTINGS-SPEC.md)** - Complete Settings Page (12-16 hrs)
  - Phase 5A: Basic Settings (2-3 hrs) - Profile, Notifications
  - Phase 5B: User Management (3-4 hrs) - Users, Offices
  - Phase 5C: System Configuration (4-5 hrs) - Quickbase, SLAs
  - Phase 5D: Audit Logs (3-4 hrs) - Compliance, Export
  - Full component code, API endpoints, prompts

### Reference
- **[TRAYCER-UI-SPEC.md](./TRAYCER-UI-SPEC.md)** - Original UI specification
- **[quickbase-config.json](./quickbase-config.json)** - 92 field mappings

---

## 🎯 What to Build Next

### Option 1: Projects List (Recommended) ⭐
**Why**: Most frequently used page, critical for daily workflow
**Time**: 3-4 hours
**Guide**: [PHASE-ALIGNMENT.md](./PHASE-ALIGNMENT.md) (lines 352-750)

**What You Get**:
- Horizontal 7-milestone traffic lights
- 9 filter views (All, My Projects, On Hold, Urgent, etc.)
- Search by customer/address/project ID
- Expandable rows with quick actions
- PPW display, project age indicators

### Option 2: Basic Settings (Also High Priority)
**Why**: All users need profile and notification management
**Time**: 2-3 hours
**Guide**: [SETTINGS-SPEC.md](./SETTINGS-SPEC.md) (lines 2697-2776)

**What You Get**:
- Profile editing (name, email, phone)
- Password change
- Notification preferences
- Alert threshold configuration (for office leaders+)
- Test notification feature

### Option 3: Dashboard
**Why**: Landing page, high visibility, first impression
**Time**: 2-3 hours
**Guide**: [PHASE-ALIGNMENT.md](./PHASE-ALIGNMENT.md) (lines 145-350)

**What You Get**:
- 4 metric cards (total projects, on hold, urgent, completed)
- Urgent alerts list (projects needing attention)
- Recent projects table

---

## 📦 What's Already Built (Phase 3)

✅ **Project Detail View** - Fully implemented by Cursor
- Route: `/app/(dashboard)/projects/[id]/page.tsx`
- 9-milestone vertical timeline
- Traffic light status indicators
- Hold management with offline support
- Contact actions (call, text, email)
- Team members card
- Adders tracking
- All verification comments resolved

**Components**:
- `ProjectHeader.tsx`
- `MilestoneTimeline.tsx`
- `ProjectDetailsCard.tsx`
- `TeamMembersCard.tsx`
- `AddersCard.tsx`
- `HoldManagementCard.tsx`
- `ContactActionsCard.tsx`

---

## 📋 Full Project Roadmap

| Phase | Feature | Status | Time | Priority |
|-------|---------|--------|------|----------|
| 3 | Project Detail View | ✅ Done | - | - |
| 4A | Dashboard | 📋 Spec'd | 2-3 hrs | High |
| 4B | Projects List | 📋 Spec'd | 3-4 hrs | **Critical** |
| 5A | Basic Settings | 📋 Spec'd | 2-3 hrs | High |
| 5B | User Management | 📋 Spec'd | 3-4 hrs | Medium |
| 5C | System Config | 📋 Spec'd | 4-5 hrs | Low |
| 5D | Audit Logs | 📋 Spec'd | 3-4 hrs | Low |
| - | Holds Page | ❌ Not Spec'd | 2-3 hrs | Medium |
| - | Analytics Page | ❌ Not Spec'd | 4-6 hrs | Low |

**Total Remaining**: 28-36 hours (10-14 hours with AI assistance)

---

## 🚀 How to Use These Specs

### For Traycer/Cursor AI

Each specification includes **copy-paste prompts** designed for AI assistants:

1. Open the relevant spec file
2. Find the "Implementation Prompts" section
3. Copy the entire prompt
4. Paste into Traycer/Cursor
5. Let AI generate the code

**Example**: To build Projects List:
```bash
# 1. Open PHASE-ALIGNMENT.md
# 2. Go to lines 752-850 (Phase 4B prompt)
# 3. Copy the entire prompt block
# 4. Paste into Cursor/Traycer
# 5. AI will generate all components and queries
```

### For Manual Implementation

Each spec includes:
- Complete component code (copy-paste ready)
- API endpoint implementations
- TypeScript interfaces
- Database schemas
- Integration points
- Acceptance criteria

---

## 🗂️ File Structure

```
docs/
├── README.md                    # This file - navigation guide
├── IMPLEMENTATION-STATUS.md     # ⭐ Current progress & next steps
├── PHASE-ALIGNMENT.md          # Phase 4A & 4B (Dashboard + Projects List)
├── SETTINGS-SPEC.md            # Phase 5A-5D (Complete Settings page)
├── TRAYCER-UI-SPEC.md          # Original UI specification
└── quickbase-config.json       # 92 Quickbase field mappings

app/
├── (dashboard)/
│   ├── page.tsx                # Dashboard (needs rebuild for Phase 4A)
│   ├── projects/
│   │   ├── page.tsx            # Projects List (needs creation for Phase 4B)
│   │   └── [id]/page.tsx       # ✅ Project Detail (Phase 3 - DONE)
│   ├── settings/
│   │   └── page.tsx            # Settings (needs creation for Phase 5)
│   ├── holds/page.tsx          # Holds (needs creation)
│   └── analytics/page.tsx      # Analytics (needs creation)

components/
├── projects/                   # ✅ Phase 3 components (DONE)
│   ├── ProjectHeader.tsx
│   ├── MilestoneTimeline.tsx
│   ├── ProjectDetailsCard.tsx
│   ├── TeamMembersCard.tsx
│   ├── AddersCard.tsx
│   ├── HoldManagementCard.tsx
│   └── ContactActionsCard.tsx
├── dashboard/                  # Phase 4A (needs creation)
│   ├── DashboardMetrics.tsx
│   ├── UrgentAlerts.tsx
│   └── RecentProjects.tsx
├── projects-list/              # Phase 4B (needs creation)
│   ├── TrafficLightPipeline.tsx
│   ├── FilterChips.tsx
│   ├── SearchBar.tsx
│   ├── ProjectRow.tsx
│   ├── ProjectTableView.tsx
│   ├── PPWDisplay.tsx
│   ├── ProjectAgeIndicator.tsx
│   └── HoldBanner.tsx
└── settings/                   # Phase 5 (needs creation)
    ├── ProfileTab.tsx
    ├── NotificationsTab.tsx
    ├── UsersTab.tsx
    ├── OfficesTab.tsx
    ├── SystemTab.tsx
    └── AuditLogsTab.tsx

lib/
├── utils/
│   ├── milestones.ts           # ✅ 9-milestone logic (DONE)
│   ├── traffic-lights.ts       # Phase 4B (needs creation)
│   └── formatters.ts           # ✅ Date/currency/phone (DONE)
├── types/
│   ├── project.ts              # ✅ QuickbaseProject (DONE)
│   ├── user.ts                 # Phase 5 (needs creation)
│   ├── office.ts               # Phase 5B (needs creation)
│   ├── audit.ts                # Phase 5D (needs creation)
│   └── settings.ts             # Phase 5C (needs creation)
└── constants/
    └── fieldIds.ts             # ✅ 92 field mappings (DONE)
```

---

## 🎨 Design System

### Colors (Traffic Lights)
- 🟢 **Green** (On Track): `bg-green-100 text-green-800`
- 🟡 **Yellow** (Warning): `bg-yellow-100 text-yellow-800`
- 🔴 **Red** (Overdue): `bg-red-100 text-red-800`
- ⚪ **Gray** (Not Started): `bg-gray-100 text-gray-800`

### Role Badge Colors
- **Super Admin**: `variant="destructive"` (red)
- **Regional**: `variant="default"` (blue)
- **Office Leader**: `variant="secondary"` (gray)
- **Closer/Setter**: `variant="outline"` (white)

### Responsive Breakpoints
- Mobile: 320px - 768px
- iPad Pro: 768px - 1024px ⭐ **Primary target**
- Desktop: 1024px+

### UI Component Library
Using **shadcn/ui** components:
- Card, Button, Input, Badge
- Table, Select, Switch, Tabs
- Dialog, AlertDialog, Popover
- Toast (via sonner)
- Calendar

---

## 🔐 Authentication & Roles

### User Roles (5 levels)
1. **Closer** - Sales rep who closed the deal
2. **Setter** - Sales rep who set the appointment
3. **Office Leader** - Manages one office
4. **Regional** - Manages multiple offices
5. **Super Admin** - Full system access

### Role-Based Access
- **Dashboard**: All roles
- **Projects**: All roles (filtered by role)
- **Holds**: All roles
- **Settings (Profile/Notifications)**: All roles
- **Settings (Users/Offices/System)**: Super Admin only
- **Analytics**: Office Leader, Regional, Super Admin

---

## 📊 Key Concepts

### 7 vs 9 Milestones

**7-Milestone Traffic Lights** (Projects List):
1. Survey Scheduled/Completed
2. Permit Submitted
3. NEM Submitted
4. Installation Completed
5. Inspection
6. PTO Approved
7. (HOA - conditional)

**9-Milestone Timeline** (Project Detail):
1. Intake
2. Survey
3. Design
4. HOA (conditional)
5. Permit
6. NEM
7. Install
8. Verification
9. Inspection
10. PTO

**Why Both?**
- 7 milestones for **quick scanning** in list view
- 9 milestones for **deep analysis** in detail view

### Traffic Light Logic
- **Green**: On track (< 75% of SLA)
- **Yellow**: Warning (75-100% of SLA)
- **Red**: Overdue (> 100% of SLA)
- **Gray**: Not started yet

---

## 🛠️ Development Commands

```bash
# Install dependencies
npm install

# Run development server
npm run dev

# Run type checking
npm run typecheck

# Run tests
npm test

# Build for production
npm run build

# Start production server
npm start
```

---

## 📖 Helpful Links

### Internal Documentation
- [IMPLEMENTATION-STATUS.md](./IMPLEMENTATION-STATUS.md) - Current status
- [PHASE-ALIGNMENT.md](./PHASE-ALIGNMENT.md) - Dashboard & Projects List specs
- [SETTINGS-SPEC.md](./SETTINGS-SPEC.md) - Settings page specs

### External Resources
- [Next.js 14 Docs](https://nextjs.org/docs)
- [TanStack Query Docs](https://tanstack.com/query/latest)
- [shadcn/ui Components](https://ui.shadcn.com/)
- [Quickbase REST API](https://developer.quickbase.com/)

---

## ✅ Quick Checklist for Each Phase

Before marking a phase as complete, verify:

- [ ] All components implemented
- [ ] All API endpoints working
- [ ] Types defined in `lib/types/`
- [ ] Error handling in place
- [ ] Toast notifications working
- [ ] Responsive on iPad Pro
- [ ] Role-based access enforced
- [ ] Optimistic updates (where applicable)
- [ ] Loading states handled
- [ ] Tests passing (if applicable)

---

## 🎯 Success Metrics

### User Experience
- Projects list loads in < 2 seconds
- Detail view loads in < 1 second
- Hold actions work offline
- Search results instant (< 500ms)
- Mobile responsive (iPad Pro)

### Code Quality
- TypeScript strict mode enabled
- No `any` types
- All props typed
- Error boundaries in place
- Consistent naming conventions

### Feature Completeness
- All 92 fields mapped correctly
- All 9 milestones calculated
- All 5 roles enforced
- All CRUD operations working
- Offline support functional

---

## 📞 Questions?

If you need clarification on any specification:

1. **Check the relevant spec file first**:
   - Phase 4A/4B → `PHASE-ALIGNMENT.md`
   - Phase 5A-5D → `SETTINGS-SPEC.md`
   - Current status → `IMPLEMENTATION-STATUS.md`

2. **Look for the "Implementation Prompts" section**:
   - Each spec has copy-paste ready prompts
   - Prompts include all context needed

3. **Review the component code**:
   - Full implementations provided in specs
   - Can copy-paste directly or use as reference

---

**Happy Building! 🚀**
