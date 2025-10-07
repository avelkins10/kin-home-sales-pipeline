# Kin Home Sales Pipeline App - Master Requirements Document

## Executive Summary

**Mission**: Replace clunky Quickbase interface with a modern, mobile-first PWA that gives sales reps transparent visibility into their pipeline while keeping operations accountable.

**Core Philosophy**: Empower sales with transparency, not anxiety. Show what they control, what to expect, and when to escalate.

---

## Critical Stats (From Data Analysis)

- **21.4%** of projects on hold (major bottleneck)
- **14.6%** cancellation rate
- **18.6%** completion rate
- **91 days** median cycle time (135 average)
- **67.8%** of projects have adders
- **67%** of adders pending review
- **50-54%** of projects have install date data (most reliable)

---

## 1. Technical Architecture

### Platform
**Next.js 14 PWA** (Progressive Web App)
- Works on iPad, iPhone, Android, Desktop
- No app store deployment needed
- Installable to home screen
- Offline-capable with service workers

### Tech Stack
```typescript
{
  frontend: "Next.js 14 App Router + TypeScript",
  styling: "Tailwind CSS + shadcn/ui",
  state: "Zustand + TanStack Query (React Query)",
  database: "Neon PostgreSQL (caching/offline)",
  auth: "Email/password (custom, matched to QB email)",
  api: "Quickbase REST API v1",
  realtime: "Polling (30s) + Quickbase webhooks",
  deployment: "Vercel",
  monitoring: "Sentry + Vercel Analytics"
}
```

### Data Flow
```
Quickbase (Source of Truth)
    ↓
Neon PostgreSQL (Cache + Offline)
    ↓
Next.js API Routes (Business Logic)
    ↓
React Query (Client State + Caching)
    ↓
UI Components
```

---

## 2. User Roles & Permissions

### Role Hierarchy
```typescript
interface UserRole {
  superAdmin: {
    access: "all projects, all offices",
    permissions: [
      "configure SLAs",
      "manage users", 
      "view all metrics",
      "system settings"
    ]
  },
  regional: {
    access: "multiple offices (configurable)",
    permissions: [
      "view office metrics",
      "escalate holds",
      "view team performance"
    ]
  },
  officeLeader: {
    access: "their office + their own projects",
    permissions: [
      "view office dashboard",
      "team performance",
      "escalate holds"
    ],
    configurable: "which offices they can see"
  },
  closer: {
    access: "only projects they closed",
    permissions: [
      "view their projects",
      "update notes",
      "cancel own projects",
      "complete tasks",
      "escalate issues"
    ]
  },
  setter: {
    access: "only projects they set",
    permissions: [
      "view their projects",
      "update notes",
      "complete tasks"
    ]
  }
}
```

### Permission Matrix
| Action | Closer | Setter | Office Leader | Regional | Super Admin |
|--------|--------|--------|--------------|----------|-------------|
| View own projects | ✅ | ✅ | ✅ | ✅ | ✅ |
| View office projects | ❌ | ❌ | ✅ | ✅ | ✅ |
| Cancel own project | ✅ | ❌ | ✅ | ✅ | ✅ |
| Configure SLAs | ❌ | ❌ | ❌ | ❌ | ✅ |
| Manage users | ❌ | ❌ | ❌ | ❌ | ✅ |
| Escalate holds | ✅ | ✅ | ✅ | ✅ | ✅ |

---

## 3. Authentication System

### Login Flow
```typescript
1. User enters email (must match Quickbase email)
2. User enters password (stored securely, hashed)
3. System validates credentials
4. Fetch user profile from Quickbase (closerId/setterId)
5. Determine role and permissions
6. Generate JWT token
7. Store in HTTP-only cookie
8. Redirect to dashboard
```

### User Profile Structure
```typescript
interface UserProfile {
  id: string;
  email: string;
  name: string;
  role: 'closer' | 'setter' | 'officeLeader' | 'regional' | 'superAdmin';
  quickbaseUserId: string;
  offices: string[]; // For leaders/regional
  phone?: string;
}
```

---

## 4. Core Features

### 4.1 Dashboard (Home Screen)

#### Sales Rep Dashboard
```typescript
interface RepDashboard {
  header: {
    greeting: "Good morning, John",
    lastSync: "2 minutes ago"
  },
  
  metrics: {
    activeProjects: 12,
    onHold: 2,          // ⚠️ Highlight
    installingThisWeek: 3,
    completedThisMonth: 8,
    
    commission: {
      thisMonth: 12450,
      pending: 8200,
      ytd: 145000
    }
  },
  
  needsAttention: [
    {
      type: 'hold',
      projectId: 358,
      customer: "Smith Family",
      reason: "Permit delay - AHJ backlog",
      daysOnHold: 12,
      action: "Contact customer to set expectations"
    },
    {
      type: 'task',
      projectId: 412,
      customer: "Jones Residence",
      task: "Confirm install date with customer",
      dueDate: "Today"
    }
  ],
  
  upcomingInstalls: [
    {
      date: "Mon, Jan 15",
      customer: "Johnson",
      systemSize: 5.2,
      address: "123 Main St"
    }
  ]
}
```

#### Leader Dashboard
```typescript
interface LeaderDashboard {
  officeMetrics: {
    office: "Phoenix Office",
    totalProjects: 45,
    onHoldRate: 18.2,    // vs company avg 21.4%
    completionRate: 22.1, // vs company avg 18.6%
    avgCycleTime: 87      // days
  },
  
  teamPerformance: [
    {
      rep: "John Smith",
      active: 12,
      onHold: 2,
      completedMTD: 8,
      commissionMTD: 12450,
      rank: 3
    }
  ],
  
  projectsAtRisk: [
    // Projects >80 days old
  ],
  
  topPerformers: [
    // Top 3 reps this month
  ]
}
```

### 4.2 Project List View

#### Filters & Search
```typescript
interface ProjectFilters {
  search: string,              // Customer name or project ID
  status: 'all' | 'active' | 'on-hold' | 'installing' | 'completed' | 'cancelled',
  dateRange: '7d' | '30d' | '90d' | 'ytd' | 'custom',
  milestone: 'survey' | 'design' | 'permit' | 'install' | 'pto',
  hasBlockers: boolean,
  office: string[]             // For leaders
}
```

#### Project Card Display
```typescript
interface ProjectCard {
  // Header
  customerName: string,
  projectId: number,
  status: {
    text: "Active - On Hold" | "Active" | "Completed",
    color: "red" | "green" | "yellow",
    badge: true
  },
  priority: "Insane" | "Urgent" | "Normal",
  
  // Timeline
  daysSinceSale: 45,
  currentMilestone: "Permitting",
  nextMilestone: "Installation",
  expectedDate: "Est. Feb 1",
  onTrack: boolean,
  
  // System Details
  systemSize: "7.2 kW",
  soldPPW: 2.85,
  commissionablePPW: 2.45,
  
  // Adders (if any)
  adders: [
    { name: "Encharge 10kW Battery", value: 12500, type: "system" },
    { name: "Perfect Power Box", value: 2250, type: "system" }
  ],
  totalAdderValue: 14750,
  
  // Alerts
  alerts: [
    { type: "hold", message: "On hold for 12 days" },
    { type: "task", message: "1 task due today" }
  ],
  
  // Quick Actions
  actions: [
    { icon: "phone", label: "Call", action: "tel:+1234567890" },
    { icon: "message", label: "Text", action: "sms:+1234567890" },
    { icon: "eye", label: "View Details" }
  ]
}
```

### 4.3 Project Detail View

#### Structure
```typescript
interface ProjectDetail {
  // Header Section
  header: {
    customerName: string,
    customerPhone: string,        // tel: link
    customerAddress: string,
    projectId: number,
    status: string,
    priority: string,
    salesDate: Date,
    daysInPipeline: number
  },
  
  // Timeline Visualization
  timeline: {
    milestones: [
      {
        name: "Intake",
        status: "completed" | "in-progress" | "not-started" | "blocked",
        submittedDate?: Date,
        approvedDate?: Date,
        completedDate?: Date,
        slaTarget: 7,           // days from previous milestone
        actualDays: 5,
        onTrack: true
      },
      // ... 9 milestones total
    ],
    
    visual: "Recreate Status Bar with actual dates"
  },
  
  // System & Financial Details
  system: {
    size: "7.2 kW",
    panels: 24,
    inverter: "Enphase IQ8+",
    basePrice: 20520,
    
    ppw: {
      gross: 2.85,
      net: 2.45,
      sold: 2.85,
      commissionable: 2.45,
      dealerFee: 0.15
    }
  },
  
  // Adders Section
  adders: [
    {
      name: "Encharge 10kW Battery",
      cost: 12500,
      type: "system",
      category: "Battery Storage",
      status: "approved",        // pending-review | approved | rejected
      requiredForInstall: true
    }
  ],
  
  // Holds/Blockers Section (prominent if present)
  holds: [
    {
      category: "Permitting",
      reason: "AHJ backlog - waiting for plan review",
      placedBy: "Operations - Sarah Johnson",
      placedDate: Date,
      daysOnHold: 12,
      customerImpact: "Install may be delayed by 2-3 weeks",
      salesAction: "Contact customer to set expectations",
      expectedResolution: "Jan 20, 2025"
    }
  ],
  
  // Team Section
  team: {
    closer: { name: string, phone: string },
    setter: { name: string, phone: string },
    projectCoordinator: { name: string, phone: string }
  },
  
  // Tasks Section
  tasks: [
    {
      id: number,
      type: "Project Update Call",
      status: "not-started" | "pending" | "completed",
      assignedTo: string,
      dueDate: Date,
      canComplete: boolean  // Based on user permissions
    }
  ],
  
  // Notes/Comments Thread
  notes: [
    {
      id: number,
      author: string,
      role: "sales" | "operations",
      timestamp: Date,
      content: string
    }
  ],
  
  // Quick Actions
  actions: {
    callCustomer: "tel:+1234567890",
    textCustomer: "sms:+1234567890",
    addNote: () => void,
    escalateHold: () => void,
    cancelProject: () => void    // If own project
  }
}
```

### 4.4 Milestone Timeline Visualization

**Recreate Status Bar with Actual Dates**

```typescript
interface MilestoneVisualization {
  style: "horizontal progress bar",
  
  milestones: [
    {
      name: "Intake",
      icon: "📋",
      status: "completed",
      color: "#10b981",      // green
      dates: {
        completed: "Dec 15, 2024",
        daysFromSale: 5
      }
    },
    {
      name: "Survey",
      icon: "🏠",
      status: "completed",
      color: "#10b981",
      dates: {
        scheduled: "Dec 18, 2024",
        completed: "Dec 20, 2024",
        daysFromSale: 10
      }
    },
    {
      name: "Design",
      icon: "📐",
      status: "completed",
      color: "#10b981",
      dates: {
        submitted: "Dec 28, 2024",
        approved: "Jan 3, 2025",
        daysFromSale: 23
      }
    },
    {
      name: "Permit",
      icon: "📄",
      status: "in-progress",
      color: "#f59e0b",      // yellow
      dates: {
        submitted: "Jan 5, 2025",
        slaTarget: "Jan 25, 2025",
        daysInProgress: 10
      }
    },
    {
      name: "Install",
      icon: "⚡",
      status: "not-started",
      color: "#6b7280",      // gray
      dates: {
        estimated: "Feb 1, 2025"
      }
    }
    // ... etc
  ],
  
  indicators: {
    completed: "✓ with green background",
    inProgress: "⏳ with yellow background, show days",
    notStarted: "○ with gray background",
    blocked: "⚠️ with red background"
  }
}
```

**Visual Design**:
```
✅ Intake → ✅ Survey → ✅ Design → ⏳ Permit → ○ Install → ○ PTO
Dec 15      Dec 20       Jan 3       (Day 10)     Est Feb 1
```

### 4.5 Hold/Blocker Management

#### Hold Categories (Predefined Dropdown)
```typescript
const holdCategories = [
  "Customer - Unavailable",
  "Customer - Requested Delay",
  "Customer - Financial Issues",
  "Permitting - AHJ Delay",
  "Permitting - Missing Documents",
  "Design - Technical Issues",
  "Design - Customer Changes",
  "Financing - Pending Approval",
  "Financing - Credit Issues",
  "Adder - Pending Review",
  "Adder - Rejected, Needs Revision",
  "Installation - Weather Delay",
  "Installation - Scheduling Conflict",
  "HOA - Pending Approval",
  "Utility - NEM Processing",
  "Other - See Notes"
];
```

#### Hold Interface
```typescript
interface HoldManagement {
  // On project detail page
  holdAlert: {
    visible: boolean,
    prominence: "top of page, red banner",
    content: {
      category: string,
      reason: string,
      daysOnHold: number,
      placedBy: string,
      customerImpact: string,
      salesAction: string
    },
    actions: [
      "Add Note",
      "Escalate to Operations",
      "Contact Customer (call/text)"
    ]
  },
  
  // Dashboard hold widget
  holdsDashboard: {
    totalOnHold: number,
    byCategory: { category: string, count: number }[],
    avgDaysOnHold: number,
    needsAttention: "Projects on hold >14 days"
  }
}
```

### 4.6 Project Cancellation Flow

```typescript
interface CancellationFlow {
  trigger: "Cancel Project button (only on own projects)",
  
  modal: {
    title: "Cancel Project",
    warning: "This will change status to 'Pending Cancel' and notify operations.",
    
    form: {
      reason: {
        type: "dropdown",
        options: [
          "Customer - Financial",
          "Customer - Changed Mind",
          "Customer - Timeline Too Long",
          "System Design Issues",
          "Better Offer Elsewhere",
          "HOA Rejection",
          "Permit Rejection",
          "Other"
        ],
        required: true
      },
      
      notes: {
        type: "textarea",
        placeholder: "Additional details (optional)",
        maxLength: 500
      }
    },
    
    buttons: [
      { label: "Cancel Project", style: "danger" },
      { label: "Nevermind", style: "secondary" }
    ]
  },
  
  actions: {
    updateQuickbase: {
      field_255_status: "Pending Cancel",
      field_cancellation_date: Date.now(),
      field_cancellation_reason: formData.reason,
      field_cancelled_by: currentUser.id
    },
    
    createTask: {
      tableId: "br9kwm8q9",
      taskType: "Cancellation Review",
      assignedTo: "Operations Manager",
      priority: "High",
      description: `${customer} - ${reason}`,
      linkedProject: projectId
    },
    
    notification: {
      type: "email",
      to: "operations@kinhome.com",
      subject: "Project Cancellation - Pending Review",
      body: "..."
    }
  }
}
```

### 4.7 Task Management

#### Task Types (from Quickbase)
- Adder Audit
- Funding
- Permit
- Pre-Intake
- Project Update Call
- Site Survey

#### Task Interface
```typescript
interface TaskManagement {
  display: "List on project detail page",
  
  taskCard: {
    type: string,
    status: "not-started" | "pending" | "completed",
    assignedTo: string,
    dueDate: Date,
    description: string,
    
    actions: [
      {
        label: "Mark Complete",
        visible: "if assigned to current user",
        action: async () => {
          // Update Quickbase immediately
          await updateTask(taskId, { status: "completed" });
          // Optimistic UI update
        }
      }
    ]
  },
  
  permissions: {
    canComplete: "Only if task assigned to current user",
    canCreate: false,  // Phase 2
    canReassign: false // Phase 2
  },
  
  sync: "Immediate push to Quickbase on completion"
}
```

### 4.8 Adder Display

```typescript
interface AdderDisplay {
  location: "Project detail page, below system specs",
  
  adderCard: {
    name: string,           // "Encharge 10kW Battery"
    cost: number,           // 12500
    type: "system" | "value",
    category: string,       // "Battery Storage"
    
    // Approval status (visible but not actionable by rep)
    status: "pending-review" | "approved" | "rejected",
    reviewedBy: "Project Coordinator", // If status = approved
    
    // Installation requirement
    requiredForInstall: boolean,
    
    // Commission impact
    commissionable: boolean,
    commissionAmount: number
  },
  
  summary: {
    totalSystemAdders: number,
    totalValueAdders: number,
    totalCost: number,
    ppwImpact: number  // How adders affect overall PPW
  },
  
  note: "Reps can see all adders and costs, but cannot approve/reject"
}
```

### 4.9 Notes/Comments System

**Option A Implementation (Recommended)**

```typescript
interface CommentsSystem {
  display: "Threaded comments on project detail page",
  
  comment: {
    id: number,
    author: string,
    role: "sales" | "operations",
    timestamp: Date,
    content: string,
    
    // Visual distinction
    style: {
      sales: "blue border",
      operations: "green border"
    }
  },
  
  newComment: {
    form: {
      textarea: "max 1000 characters",
      visibleTo: ["Operations", "Sales Team", "Everyone"], // Dropdown
      buttons: ["Post Comment", "Cancel"]
    }
  },
  
  sync: {
    to: "Quickbase notes field or separate Comments table",
    method: "Immediate push on submit",
    realtime: "Poll every 30s for new comments"
  },
  
  notifications: {
    email: "Notify operations when sales comments",
    inApp: "Badge count on project card"
  }
}
```

### 4.10 Call/Text Customer Integration

```typescript
interface CustomerContact {
  callButton: {
    action: "tel:+1234567890",
    behavior: "Open device native phone app",
    tracking: "Optional: Log call attempt to Quickbase"
  },
  
  textButton: {
    action: "sms:+1234567890",
    behavior: "Open device native SMS app",
    prePopulated: false, // Phase 2: Pre-written templates
    tracking: "Optional: Log text sent to Quickbase"
  },
  
  display: [
    "Project card quick action",
    "Project detail header",
    "Hold alert (if customer action needed)"
  ]
}
```

---

## 5. Admin Features

### 5.1 SLA Configuration (Super Admin Only)

```typescript
interface SLAConfiguration {
  access: "Super Admin only",
  
  interface: {
    title: "Milestone SLA Settings",
    description: "Set target days for each milestone to manage rep expectations",
    
    milestones: [
      {
        name: "Intake",
        targetDays: 7,
        warningDays: 5,      // Show yellow warning at this point
        description: "Days from project created to intake complete"
      },
      {
        name: "Survey",
        targetDays: 14,
        warningDays: 10,
        description: "Days from intake to survey completed"
      },
      {
        name: "Design",
        targetDays: 21,
        warningDays: 18,
        description: "Days from survey to design approved"
      },
      {
        name: "Permit",
        targetDays: 30,
        warningDays: 25,
        description: "Days from design to permit approved"
      },
      {
        name: "NEM",
        targetDays: 21,
        warningDays: 18,
        description: "Days from permit to NEM approved"
      },
      {
        name: "Installation",
        targetDays: 14,
        warningDays: 10,
        description: "Days from NEM to install completed"
      },
      {
        name: "Inspection",
        targetDays: 7,
        warningDays: 5,
        description: "Days from install to inspection passed"
      },
      {
        name: "PTO",
        targetDays: 14,
        warningDays: 10,
        description: "Days from inspection to PTO received"
      }
    ],
    
    buttons: ["Save Changes", "Reset to Defaults"]
  },
  
  storage: "Neon PostgreSQL settings table",
  
  usage: "Calculate on-track/delayed status in timeline visualization"
}
```

### 5.2 Office Configuration (Super Admin)

```typescript
interface OfficeConfiguration {
  access: "Super Admin only",
  
  interface: {
    title: "Office & Leader Management",
    
    offices: [
      {
        id: "PHX",
        name: "Phoenix Office",
        region: "Southwest",
        leaders: ["user123", "user456"]
      }
    ],
    
    leaders: [
      {
        userId: "user123",
        name: "Sarah Johnson",
        role: "officeLeader",
        offices: ["PHX"],          // Can see Phoenix
        canManage: ["PHX"]         // Can manage Phoenix
      },
      {
        userId: "user789",
        name: "Mike Chen",
        role: "regional",
        offices: ["PHX", "TUC", "LV"],  // Can see 3 offices
        canManage: ["PHX", "TUC", "LV"] // Can manage all 3
      }
    ],
    
    actions: [
      "Add Office",
      "Edit Office",
      "Assign Leader",
      "Configure Permissions"
    ]
  }
}
```

---

## 6. Data Sync & Offline Strategy

### 6.1 Sync Strategy

```typescript
interface SyncStrategy {
  // Initial Load
  onLogin: {
    action: "Full sync of user's projects",
    method: "Quickbase API query with user filter",
    storage: "Neon PostgreSQL + IndexedDB",
    duration: "~3-5 seconds for 100 projects"
  },
  
  // Realtime Updates
  polling: {
    interval: 30000, // 30 seconds
    endpoint: "/api/sync/changes",
    method: "Query projects modified since last sync",
    fields: ["status", "dates", "holds", "tasks", "notes"],
    optimistic: true // Show changes immediately, sync in background
  },
  
  // Critical Changes (via Webhooks)
  webhooks: {
    events: [
      "project_status_changed",
      "hold_added",
      "task_assigned",
      "comment_added"
    ],
    action: "Push notification + immediate refresh"
  },
  
  // User Actions
  mutations: {
    method: "Immediate push to Quickbase",
    fallback: "Queue in IndexedDB if offline",
    retry: "Exponential backoff on failure"
  }
}
```

### 6.2 Offline Support

```typescript
interface OfflineStrategy {
  storage: {
    projects: "IndexedDB (last 90 days)",
    settings: "localStorage",
    auth: "HTTP-only cookie + localStorage flag"
  },
  
  capabilities: {
    view: "All cached project data",
    search: "Local search in cached data",
    filter: "Client-side filtering",
    
    readOnly: true, // Can't update while offline
    
    queue: {
      actions: ["update status", "add note", "complete task"],
      storage: "IndexedDB queue",
      syncOnReconnect: true
    }
  },
  
  indicators: {
    offlineBanner: "🔴 Offline - showing cached data",
    lastSync: "Last synced: 5 minutes ago",
    queuedActions: "2 actions pending sync"
  }
}
```

---

## 7. Performance Requirements

### 7.1 Load Times

```typescript
const performanceTargets = {
  initialLoad: {
    target: "< 2 seconds",
    metric: "First Contentful Paint",
    measurement: "Lighthouse score >90"
  },
  
  dashboardRefresh: {
    target: "< 1 second",
    metric: "Time to Interactive"
  },
  
  projectDetail: {
    target: "< 1.5 seconds",
    metric: "Full page load"
  },
  
  search: {
    target: "< 500ms",
    metric: "Results displayed"
  },
  
  apiCalls: {
    target: "< 1 second",
    metric: "95th percentile response time"
  }
};
```

### 7.2 Quickbase API Limits

```typescript
const apiStrategy = {
  limits: {
    requestsPerSecond: 10,
    requestsPerDay: 100000 // Verify with QB docs
  },
  
  optimization: {
    batching: "Combine multiple field updates in single request",
    caching: "Cache in Neon for 5 minutes",
    debouncing: "Debounce search queries by 300ms",
    pagination: "Fetch 50 projects at a time"
  },
  
  errorHandling: {
    "429": "Exponential backoff (1s, 2s, 4s, 8s)",
    "401": "Force re-authentication",
    "500": "Retry 3 times, then show error"
  }
};
```

---

## 8. UI/UX Guidelines

### 8.1 Design System

```typescript
const designSystem = {
  colors: {
    primary: "#2563eb",      // Blue for actions
    success: "#10b981",      // Green for completed/on-track
    warning: "#f59e0b",      // Yellow for attention needed
    danger: "#ef4444",       // Red for critical/blocked
    neutral: "#6b7280",      // Gray for waiting/not started
    
    backgrounds: {
      light: "#ffffff",
      gray: "#f3f4f6",
      dark: "#111827"
    }
  },
  
  typography: {
    fontFamily: "Inter",
    sizes: {
      xs: "12px",
      sm: "14px",
      base: "16px",
      lg: "18px",
      xl: "20px",
      "2xl": "24px",
      "3xl": "30px"
    }
  },
  
  spacing: {
    unit: 4,
    scale: [4, 8, 12, 16, 20, 24, 32, 40, 48, 64]
  },
  
  borderRadius: {
    sm: "4px",
    md: "8px",
    lg: "12px",
    full: "9999px"
  }
};
```

### 8.2 Responsive Breakpoints

```typescript
const breakpoints = {
  mobile: "< 640px",      // iPhone (secondary support)
  tablet: "640-1024px",   // iPad Portrait (primary)
  desktop: "> 1024px",    // iPad Landscape + Desktop (primary)
  
  designPriority: {
    primary: "iPad (both orientations) + Desktop",
    secondary: "iPhone and Android phones",
    approach: "Desktop-first with responsive adaptations"
  },
  
  optimization: {
    mobile: "Stack vertically, larger tap targets (56px), simplified views",
    tablet: "2-column layouts, 44px tap targets, full feature set",
    desktop: "3-column layouts, traditional sizing, hover enhancements optional"
  },
  
  layoutStrategy: {
    dashboard: "Grid layouts that collapse on mobile",
    projectList: "Cards that stack on mobile, 2-col on tablet, 3-col on desktop",
    projectDetail: "Sidebar on desktop, stacked sections on mobile"
  }
};
```

### 8.3 Component Patterns

```typescript
const componentPatterns = {
  cards: {
    style: "Elevated with shadow, rounded corners",
    padding: "16px",
    spacing: "12px between cards"
  },
  
  buttons: {
    primary: "Solid blue, white text",
    secondary: "Outline, blue border",
    danger: "Solid red, white text",
    minHeight: "44px",
    minWidth: "88px"
  },
  
  inputs: {
    height: "44px",
    border: "1px solid gray-300",
    focusBorder: "2px solid blue-500",
    placeholder: "gray-400"
  },
  
  statusBadges: {
    shape: "Rounded pill",
    padding: "4px 12px",
    fontSize: "12px",
    fontWeight: "600"
  }
};
```

---

## 9. Critical Quickbase Field Mappings

### 9.1 Core Project Fields

```typescript
const QB_FIELDS = {
  // Identity
  recordId: 3,
  projectId: 11,
  
  // Customer
  customerName: 145,
  customerAddress: 146,
  customerPhone: 148,
  
  // Status
  projectStatus: 255,
  statusBar: 301,        // HTML visualization
  priority: 300,
  
  // Dates - Use PRIMARY first, fall back to BACKUP
  salesDate: {
    primary: 522,        // 100% usage
    backup: [695, 948]
  },
  
  // Milestone Dates (varying usage 20-54%)
  // IMPORTANT: Don't skip "low usage" fields!
  // Low usage = milestone reached by fewer projects, NOT unreliable data
  
  // Intake (100% usage - always filled)
  intakeInstallDateTentative: { primary: 902, usage: "100%", note: "Filled at sale" },
  
  // Survey (25-27% usage - 1 in 4 projects reach this)
  surveyScheduled: { primary: 166, usage: "0.7%", note: "Skip - rarely used" },
  surveySubmitted: { primary: 164, backup: 575, usage: "26.5%", include: true },
  surveyApproved: { primary: 165, backup: 693, usage: "25.2%", include: true },
  
  // Design (24-27% usage)
  designCompleted: { primary: 315, usage: "26.2%", include: true },
  cadDesignApproved: { primary: 476, usage: "26.2%", include: true },
  engineeringSubmitted: { primary: 477, usage: "27.5%", include: true },
  designSLADeadline: { primary: 2459, usage: "25.2%", include: true, note: "SLA tracking" },
  
  // HOA (10.7% usage - INCLUDE! Only ~10% of homes need HOA)
  hoaApplicationSubmitted: { primary: 212, usage: "10.7%", include: true, note: "CRITICAL for 10% that need it" },
  hoaApplicationApproved: { primary: 213, usage: "10.7%", include: true },
  
  // Permit (21-24% usage)
  permitSubmitted: { primary: 207, backup: 709, usage: "23.5%", include: true },
  permitApproved: { primary: 208, backup: 584, usage: "21.8%", include: true },
  
  // NEM/Interconnection (12-24% usage)
  nemSubmitted: { primary: 326, backup: 716, usage: "24.2%", include: true },
  nemApproved: { primary: 327, backup: 585, usage: "22.8%", include: true },
  interconnectionSignaturesSent: { primary: 2198, usage: "16.1%", include: true },
  nemSignatureReceived: { primary: 1845, usage: "12.8%", include: true },
  
  // Install (18-54% usage - MOST RELIABLE!)
  installScheduled: { primary: 710, backup: [178, 464], usage: "54.4%", note: "MOST RELIABLE" },
  installCompleted: { primary: 534, backup: 587, usage: "52.3%", note: "MOST RELIABLE" },
  installStarted: { primary: 464, usage: "19.8%", include: true },
  installFundingSubmitted: { primary: 486, usage: "18.8%", include: true },
  installFundingReceived: { primary: 487, usage: "13.8%", include: true },
  
  // Inspection (9-19% usage - INCLUDE! Post-install milestone)
  inspectionScheduled: { primary: 226, usage: "9.7%", note: "Optional - edge case" },
  inspectionPassed: { primary: 491, usage: "19.1%", include: true, note: "ESSENTIAL post-install" },
  
  // PTO (20-21% usage - INCLUDE! THE FINAL MILESTONE)
  ptoSubmitted: { primary: 537, usage: "21.1%", include: true },
  ptoApproved: { primary: 538, usage: "20.5%", include: true, note: "FINISH LINE - must track!" },
  ptoUploadedToLender: { primary: 556, usage: "19.8%", include: true },
  
  // System Details
  systemSizeKW: 13,
  systemPrice: 133,
  
  // PPW Fields
  grossPPW: 19,           // 100% usage
  netPPW: 543,            // 94% usage
  soldGrossPPW: 2292,     // 3% usage
  soldNetPPW: 2293,       // 3% usage
  commissionablePPW: 2480, // 94% usage
  dealerFeePPW: 545,      // 94% usage
  
  // Team
  closerId: 516,
  closerName: 517,
  setterId: 329,
  setterName: 330,
  projectCoordinatorId: 819,
  projectCoordinator: 820,
  recordOwner: 4,
  
  // Holds/Blockers
  onHold: 231,           // checkbox
  holdReason: 232,       // text
  blockReason: 233,      // text
  userPlacedOnHold: 234, // user object
  dateOnHold: 235,       // date
  holdDetails: 1389,     // multi-line text
  
  // Adders
  totalAdders: 252,
  totalAdderCost: 2115,
  salesFacingAdderList: 2286,  // CRITICAL for display
  numAddersApproved: 1046,
  numNeedsReview: 2282,
  repAdderPPW: 544,
  totalAddOnPPW: 2114
};
```

### 9.2 Field Usage Strategy

```typescript
// CRITICAL: Understanding Field Usage Percentages
// Low usage ≠ unreliable field!
// 20% usage for PTO = only 20% of projects in sample are FULLY COMPLETE
// 10% usage for HOA = only 10% of homes NEED HOA approval
// These fields are ESSENTIAL for tracking those specific projects

const usageInterpretation = {
  "50%+": "Very common milestone OR early-stage field → Always include",
  "20-50%": "Mid-pipeline milestone → Always include",
  "10-20%": "Later-stage OR conditional milestone → YES, include!",
  "5-10%": "Edge cases (resubmissions, failures) → Maybe include",
  "<5%": "Rarely used, legacy, or test fields → Skip"
};

const fieldStrategy = {
  // PRIMARY/BACKUP Logic - Try primary first, fall back if null
  primaryBackupLogic: async (fieldConfig) => {
    // Try primary field first
    let value = await getField(fieldConfig.primary);
    
    // If null and backup exists, try backup(s)
    if (!value && fieldConfig.backup) {
      const backups = Array.isArray(fieldConfig.backup) 
        ? fieldConfig.backup 
        : [fieldConfig.backup];
      
      for (const backupId of backups) {
        value = await getField(backupId);
        if (value) break;
      }
    }
    
    return value;
  },
  
  // Standard Dashboard: Include ALL fields ≥10% usage (42 fields total)
  standardDashboard: {
    coreFields: 5,      // recordId, projectId, customerName, status, salesDate
    teamFields: 6,      // closer, setter, coordinator (ID + Name)
    holdFields: 5,      // onHold, reason, date, user, details
    milestoneFields: 20,  // ALL milestones ≥10% usage (see below)
    adderFields: 3,     // count, cost, list
    financialFields: 2, // systemPrice, systemSize
    calculatedFields: 1 // projectAge
  },
  
  // Complete Dashboard: Add backup fields and edge cases (65+ fields)
  completeDashboard: "Includes all standard + backup date fields + funding milestones",
  
  // CRITICAL: Include these "low usage" fields - they're NOT unreliable!
  essentialLowUsageFields: {
    hoaApplication: {
      usage: "10.7%",
      why: "Only ~10% of homes NEED HOA approval",
      include: true,
      reasoning: "For the 10% that need it, this is CRITICAL"
    },
    inspectionPassed: {
      usage: "19.1%",
      why: "Only post-install projects have inspection",
      include: true,
      reasoning: "Essential post-install milestone"
    },
    ptoApproved: {
      usage: "20.5%",
      why: "Only 20% of sample are FULLY COMPLETE",
      include: true,
      reasoning: "This is THE FINAL MILESTONE - must track!"
    },
    installFunding: {
      usage: "13.8-18.8%",
      why: "Payment timing varies by lender",
      include: true,
      reasoning: "Tracks milestone payments (M1/M2/M3)"
    }
  },
  
  // Graceful handling of missing data
  displayStrategy: {
    ifFieldNull: "Show 'Pending' or '○ Not Started'",
    neverShow: "N/A or 'Not Available'",
    timelineDisplay: "Show all 9 milestones with status indicators",
    forConditional: "If HOA null, show 'Not Applicable' not 'Pending'"
  },
  
  calculatedFields: {
    // Only calculate if actual fields are empty
    ppw: {
      calculate: (systemPrice, systemSizeKW) => {
        return systemPrice / (systemSizeKW * 1000);
      },
      useWhen: "All PPW fields are null"
    },
    
    projectAge: {
      calculate: (salesDate) => {
        return Math.floor((Date.now() - salesDate) / (1000 * 60 * 60 * 24));
      },
      useWhen: "Always calculate client-side"
    },
    
    onTrack: {
      calculate: (currentDate, targetDays, salesDate) => {
        const actualDays = (currentDate - salesDate) / (1000 * 60 * 60 * 24);
        return actualDays <= targetDays;
      },
      useWhen: "Compare against SLA configuration"
    }
  }
};

// Sample composition explains usage percentages
const sampleBreakdown = {
  active: "41% (early/mid pipeline)",
  onHold: "21% (stalled at various stages)",
  completed: "19% (finished all milestones)",
  cancelled: "14% (never finished)"
};

// Therefore: Later milestones naturally have lower usage
// This is EXPECTED and NORMAL - not a data quality issue!
```

---

## 10. Phase 1 MVP Scope (6-8 Weeks)

### Week 1-2: Foundation
- [x] Next.js 14 project setup
- [x] Authentication system (email/password)
- [x] Quickbase API client
- [x] Database schema (Neon)
- [x] Basic UI components (shadcn/ui)

### Week 3-4: Core Features
- [x] Dashboard with metrics
- [x] Project list with filters
- [x] Project detail view
- [x] Milestone timeline visualization
- [x] Hold management interface
- [x] Task management

### Week 5-6: Adders & Polish
- [x] Adder display
- [x] Notes/comments system
- [x] Call/text integration
- [x] Project cancellation flow
- [x] Performance optimization

### Week 7-8: Testing & Deploy
- [x] User testing with 5 reps
- [x] Bug fixes
- [x] Performance optimization
- [x] Production deployment
- [x] Training documentation

---

## 11. Phase 2 Features (Future)

- Real-time chat (Sales ↔ Operations)
- Push notifications
- Custom report builder
- Bulk actions
- Advanced analytics
- Commission calculator
- Document upload
- Customer portal
- Mobile apps (iOS/Android native)

---

## 12. Success Metrics

### Primary KPIs (90 days)
- **Time to find info**: <3 seconds (baseline: unknown)
- **Daily active users**: >90% of sales team
- **Tasks completed in app**: >30% increase
- **Hold resolution time**: 50% reduction
- **App load time**: <2 seconds

### Business Impact (90 days)
- **Reduce hold rate**: 21.4% → <10%
- **Reduce cancellation rate**: 14.6% → <10%
- **Increase completion rate**: 18.6% → >30%
- **Reduce cycle time**: 91 days → <70 days

---

## 13. Files to Provide Tracer/Cursor

### Required Documentation
1. ✅ This requirements document
2. ✅ QUICKBASE-PROJECT-BRIEF.md
3. ✅ ANALYSIS-SUMMARY.md
4. ✅ ADDERS-ANALYSIS.md
5. ✅ QUICKBASE-API-REFERENCE.md
6. ✅ COMPLETE-FIELD-MAPPING.md
7. ✅ quickbase-config.json
8. ✅ Sample UI mockups (create separately)

### Project Structure
```
kin-home-pipeline/
├── app/
│   ├── (auth)/
│   │   ├── login/
│   │   └── register/
│   ├── (dashboard)/
│   │   ├── page.tsx              # Dashboard
│   │   ├── projects/
│   │   │   ├── page.tsx          # Project list
│   │   │   └── [id]/page.tsx    # Project detail
│   │   ├── holds/page.tsx        # Hold management
│   │   └── settings/page.tsx    # Settings
│   └── api/
│       ├── auth/
│       ├── projects/
│       ├── tasks/
│       ├── sync/
│       └── quickbase/
├── components/
│   ├── ui/                       # shadcn components
│   ├── dashboard/
│   ├── projects/
│   └── shared/
├── lib/
│   ├── quickbase/
│   │   ├── client.ts
│   │   ├── fields.ts
│   │   └── queries.ts
│   ├── db/
│   │   └── schema.ts
│   └── utils/
├── types/
└── public/
```

---

## 14. .cursorrules Configuration

```
# Kin Home Sales Pipeline PWA

## Project Context
Building Next.js 14 PWA to replace Quickbase interface for solar sales team.
Primary devices: iPad, iPhone, Desktop browsers.
Users: 50+ sales reps (closers/setters) + office leaders + regional managers.

## Critical Stats
- 21.4% of projects on hold (TOP PRIORITY)
- Install dates most reliable (50%+ usage)
- PPW fields 94-100% populated (use directly)
- Quickbase API: 10 req/sec limit (batch aggressively)

## Tech Stack
- Next.js 14 App Router + TypeScript strict
- Tailwind CSS + shadcn/ui
- TanStack Query + Zustand
- Neon PostgreSQL for caching
- Vercel deployment

## Core Principles
- **iPad/Desktop first** (primary devices), responsive to mobile
- Optimistic updates for instant feedback
- Offline-capable (IndexedDB + service worker)
- Load times <2 seconds
- Sales-focused transparency without panic
- Touch-optimized (44px min tap targets)

## Critical Data Understanding
- **Low usage ≠ unreliable!** 20% PTO usage = only 20% of projects completed
- Include ALL milestone fields ≥10% usage (42 fields for standard dashboard)
- HOA (10.7%), Inspection (19%), PTO (20%) are ESSENTIAL despite "low" usage
- Use PRIMARY field first, fall back to BACKUP if null
- Show "Pending" not "N/A" for missing milestone dates

## Quickbase Integration
- Use field IDs from quickbase-config.json
- Try PRIMARY field, fall back to BACKUP if null
- Parse Status Bar HTML (field 301) for visualization
- Filter projects by user role (closer/setter/leader/regional)
- Poll every 30s for updates
- Immediate push for mutations
- Handle rate limits with exponential backoff

## Field Mapping Strategy
- Install dates: PRIMARY 710, BACKUP [534, 587]
- Survey dates: PRIMARY 164/165, BACKUP 575/693
- PPW: Use actual fields (grossPPW 19, netPPW 543, commissionablePPW 2480)
- Don't calculate if fields exist

## UI/UX Rules
- Cards over tables (mobile-friendly)
- Green = completed/on-track
- Yellow = needs attention
- Red = critical/blocked
- Gray = waiting/not started
- Loading skeletons (no spinners)
- Swipe gestures for quick actions

## Role-Based Access
- Closers: Only their projects (closerId = userId)
- Setters: Only their projects (setterId = userId)
- Office Leaders: Their office + their projects (configurable)
- Regional: Multiple offices (configurable)
- Super Admin: All projects, can configure SLAs

## Never
- Don't calculate PPW if fields exist
- Don't query child tables for basic dates (parent has them)
- Don't use tables for mobile layouts
- Don't show internal ops metrics without context
- Don't panic sales reps with SLA breaches
```

---

## 15. Open Questions / Decisions Needed

### Before Starting Development

1. **Quickbase API Token**: Need production API token with proper permissions
2. **Neon Database**: Need connection string for production
3. **Email Service**: For notifications (SendGrid? AWS SES?)
4. **Phone Numbers**: Format validation for tel:/sms: links
5. **Branding**: Logo, exact color scheme, fonts
6. **Deployment**: Vercel account and domain name
7. **Error Monitoring**: Sentry DSN for production

### Design Decisions

8. **Leader Office Assignment**: Manual configuration or auto-detect from Quickbase?
9. **Hold Categories**: Final list of 10-15 predefined categories
10. **Task Types**: Any additional task types beyond the 6 identified?
11. **Commission Display**: Show exact $ amounts or just percentages?
12. **Customer Portal**: Phase 2 priority level?

---

## 16. Risk Mitigation

### Technical Risks
| Risk | Impact | Mitigation |
|------|--------|-----------|
| Quickbase API rate limits | High | Aggressive caching, batch requests |
| Offline data conflicts | Medium | Timestamp-based conflict resolution |
| Large dataset performance | Medium | Pagination, virtualization, indexing |
| Mobile browser compatibility | Low | Progressive enhancement, polyfills |

### Business Risks
| Risk | Impact | Mitigation |
|------|--------|-----------|
| Low user adoption | High | Extensive user testing, training |
| Data accuracy issues | High | Validation, error handling, logging |
| Operations pushback | Medium | Phase rollout, stakeholder buy-in |
| Feature creep | Medium | Strict MVP scope, Phase 2 backlog |

---

## Conclusion

This document provides complete specifications for building the Kin Home Sales Pipeline PWA. All field mappings, workflows, and requirements are based on actual Quickbase data analysis.

**Next Steps**:
1. Review and approve requirements
2. Set up development environment
3. Create UI mockups for key screens
4. Begin Week 1-2 foundation work
5. Schedule weekly check-ins

**Questions?** Contact: [Your Contact Info]

---

**Document Version**: 1.0
**Last Updated**: January 2025
**Status**: Ready for Development