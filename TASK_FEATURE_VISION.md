# Task Completion Feature - Vision & Implementation Guide

## Executive Summary

**Goal:** Enable sales reps to view, complete, and track intake rejection tasks directly in our app, improving the feedback loop between operations and sales.

**Current State:** When a project fails intake review, tasks are created in QuickBase (286 active task groups exist today). Reps currently complete these in QuickBase, but this creates friction and poor visibility.

**Desired State:** Reps see rejected projects with task badges in our app, can upload missing documents directly, track review status, and get notified when approved or revision is needed. Operations continues reviewing in QuickBase.

---

## Business Context

### The Problem
1. Projects get rejected during intake review due to missing/incorrect documents
2. Tasks are created in QuickBase for each missing item
3. Reps must context-switch to QuickBase to complete tasks
4. Poor visibility leads to delays and forgotten tasks
5. No proactive notifications when tasks need attention

### The Solution
**Bring task visibility and completion into our existing app** while keeping ops workflow in QuickBase.

### Success Metrics
- Reduce average task completion time from X days to Y days
- Increase first-time task approval rate
- Reduce context-switching for reps
- Improve project reactivation speed after rejection

---

## User Experience Vision

### For Sales Reps

#### Scenario 1: Project Gets Rejected
```
Rep logs into app →
  Sees notification: "Project 123 Main St needs your attention"
  Projects list shows: "📦 123 Main St - Rejected ⚠️ 3 tasks pending"
  Badge/indicator makes it obvious which projects need work
```

#### Scenario 2: Completing Tasks
```
Rep clicks project →
  Top of project detail shows task section:

  ┌─────────────────────────────────────────────────┐
  │ ⚠️ Action Required: Complete Missing Items      │
  │                                                 │
  │ Progress: 1 of 3 approved ▓▓▓▓▓░░░░░░░░        │
  │                                                 │
  │ 🔴 Finance Stip Other                           │
  │    Status: Not Started                         │
  │    📎 Drag files here or [Browse]              │
  │    Notes: [Optional text field]                │
  │    [Submit]                                     │
  │                                                 │
  │ 🟡 Missing UB                                   │
  │    Status: Needs Revision                      │
  │    Ops Feedback: "Please include all pages"    │
  │    Previous: utility_bill_v1.pdf               │
  │    📎 Upload corrected version                  │
  │    [Resubmit]                                   │
  │                                                 │
  │ ✅ Utility Bill Rejected                        │
  │    Status: Approved ✓                          │
  │    Submitted: 2 days ago                       │
  └─────────────────────────────────────────────────┘
```

Rep uploads files → Gets confirmation → Task shows "Pending Review"

#### Scenario 3: Tracking Progress
```
Rep checks back later →
  Sees notification: "1 task approved, 1 needs revision"
  Task list updates with latest status
  When all approved → Project badge changes to "Ready for Reactivation"
```

### For Operations (No Change)
- Ops continues reviewing in QuickBase
- They approve or request revisions
- Their feedback automatically shows in our app

---

## Technical Architecture

### QuickBase Table Structure

The data already exists in QuickBase. We just need to read and write to it:

```
Projects Table (br9kwm8na)
    │
    └─▶ Sales Task Groups (bu36gem4p)
            └─▶ Sales Tasks (bu36ggiht)
                    └─▶ Sales Task Submissions (bu36g8j99)
```

#### Key Tables & Fields:

**1. Sales Task Groups (bu36gem4p)**
- Purpose: Groups related tasks for a project
- Field 10: Related Project (links to Projects table)
- Field 31: # of Tasks (total count)
- Field 32: # of Unapproved Tasks (pending count)

**2. Sales Tasks (bu36ggiht)**
- Purpose: Individual tasks (e.g., "Finance Stip Other")
- Field 6: Task Group (parent link)
- Field 9: Status ("Not Started", "In Progress", "Complete")
- Field 10: Name (task description)
- Field 31: Category (task type)
- Field 13: Max Submission - Submission Status (latest status)

**3. Sales Task Submissions (bu36g8j99)**
- Purpose: Rep's submission attempts (supports resubmissions)
- Field 1: Date Created (timestamp)
- Field 6: Related Sales Task (parent link)
- Field 7: Submission Status ("Pending Approval", "Reviewed")
- Field 8: Ops Review Disposition ("Approved" / "Needs Revision")
- Field 9: File Attachments (uploaded documents)

---

## API Requirements

### Read Operations (GET)

**1. Get Tasks for a Project**
```
Query: Sales Task Groups where Related Project = {project_id}
Returns: Task groups with child tasks and submission status

Example Response:
{
  "taskGroups": [
    {
      "id": "237",
      "totalTasks": 3,
      "unapprovedTasks": 2,
      "tasks": [
        {
          "id": "1234",
          "name": "Finance Stip Other",
          "status": "Not Started",
          "category": "Finance Stip Other",
          "submissions": []
        },
        {
          "id": "1235",
          "name": "Missing UB",
          "status": "In Progress",
          "category": "Utility Bill",
          "submissions": [
            {
              "id": "5678",
              "date": "2025-10-20",
              "status": "Pending Approval",
              "disposition": null,
              "files": [{...}]
            }
          ]
        }
      ]
    }
  ]
}
```

**2. Get All Projects with Task Counts**
```
Enhanced project query to include:
- Does this project have task groups?
- How many tasks total?
- How many unapproved?
- Completion percentage

Used to show badges on project cards
```

### Write Operations (POST)

**1. Create Task Submission**
```
Endpoint: QuickBase /records API
Table: bu36g8j99 (Sales Task Submissions)

Payload:
{
  "to": "bu36g8j99",
  "data": [
    {
      "6": {"value": taskId},  // Related Sales Task
      "7": {"value": "Pending Approval"},  // Submission Status
      // Field 8 (Ops Disposition) empty until ops reviews
    }
  ]
}

Then: Upload file to field 9 using /files endpoint
```

**2. Update Task Status**
```
When rep starts working, update task status to "In Progress"
When submission created, keep as "In Progress"
When ops approves, ops updates to "Complete" in QuickBase
```

---

## UI Component Structure

### 1. Project Card Enhancement
**Where:** Projects list/grid view
**Changes:**
- Add task badge: "⚠️ 3 tasks" if tasks exist
- Add progress indicator if some tasks approved
- Visual priority for projects with pending tasks
- Filter option: "Show only projects with tasks"

**Implementation:**
- Query task groups when loading projects
- Join task count data to project objects
- Conditional rendering of task badges

### 2. Project Detail - Task Section
**Where:** Top of project detail page (above existing content)
**When:** Only shows if project has task groups
**Components:**
- Task section header with progress bar
- Task list (expandable cards)
- Each task card shows:
  - Task name and category
  - Status badge (color-coded)
  - Upload zone (if not submitted)
  - Submission history (if exists)
  - Ops feedback (if revision needed)
  - Action button (Submit/Resubmit)

**Implementation:**
- Create `TaskSection` component
- Create `TaskCard` component with file upload
- File upload with validation (PDF, images, docs)
- Show preview of uploaded files
- Display submission history timeline

### 3. Notifications
**Where:** Top nav, notification center
**Triggers:**
- New tasks assigned (project rejected)
- Task approved by ops
- Revision requested by ops
- All tasks complete (project ready)

**Implementation:**
- Poll for status changes or use webhooks
- Show badge count on notification icon
- Toast notifications for real-time updates

---

## Data Flow Examples

### Flow 1: Rep Submits a Task
```
1. User clicks "Submit" on task with file attached
2. Frontend validates file (type, size)
3. API call: Create submission record in bu36g8j99
   - Field 6 = task ID
   - Field 7 = "Pending Approval"
4. API call: Upload file to submission record field 9
5. API call: Update task status to "In Progress" (bu36ggiht)
6. Frontend updates UI:
   - Task shows "Pending Review" status
   - Progress bar updates
   - Show success message
7. Backend (optional): Send notification to ops
```

### Flow 2: Ops Reviews (Happens in QuickBase)
```
1. Ops opens QuickBase review queue
2. Views submission and attached file
3. Either:
   A. Approves: Sets field 8 = "Approved", field 9 = "Complete"
   B. Rejects: Sets field 8 = "Needs Revision", adds feedback
4. QuickBase auto-updates parent task counts
```

### Flow 3: Rep Checks Status
```
1. Rep opens project in our app
2. API query: Get tasks and submissions for project
3. Frontend sees submission with disposition = "Approved"
4. UI shows: ✅ Task name - Status: Approved
5. If all tasks approved:
   - Show celebration message
   - Hide task section or mark complete
   - Update project badge
```

### Flow 4: Rep Resubmits After Revision
```
1. Rep sees task with "Needs Revision" status
2. Reads ops feedback
3. Uploads corrected file
4. Creates NEW submission record (keeps history)
5. New submission shows "Pending Approval"
6. Rep can see both submissions in history
```

---

## QuickBase API Details

### Authentication
```
Headers:
  "QB-Realm-Hostname": "kin.quickbase.com"
  "Authorization": "QB-USER-TOKEN {token}"
  "Content-Type": "application/json"
```

### Query Records (GET tasks)
```
POST https://api.quickbase.com/v1/records/query

Body:
{
  "from": "bu36gem4p",  // Table ID
  "select": [3, 10, 31, 32],  // Field IDs
  "where": "{10.EX.12345}"  // Where clause
}
```

### Create Records (Submit task)
```
POST https://api.quickbase.com/v1/records

Body:
{
  "to": "bu36g8j99",  // Table ID
  "data": [
    {
      "6": {"value": taskId},
      "7": {"value": "Pending Approval"}
    }
  ]
}
```

### Upload File
```
POST https://api.quickbase.com/v1/files

Multipart form-data with:
- tableId: "bu36g8j99"
- recordId: submissionId
- fieldId: "9"
- file: [binary data]
```

---

## Edge Cases & Considerations

### 1. Multiple Submissions
- Tasks can have multiple submissions (resubmissions)
- Always show the latest submission status
- Allow viewing submission history
- Don't delete old submissions

### 2. Task Status Sync
- Task status is updated by both reps and ops
- Rep sets to "In Progress"
- Ops sets to "Complete" when approved
- Query latest state before showing UI

### 3. File Size & Types
- Validate file types (PDF, JPG, PNG, DOC, DOCX)
- Limit file size (e.g., 10 MB max)
- Show upload progress bar
- Handle upload errors gracefully

### 4. Network Failures
- Handle API timeouts
- Show clear error messages
- Allow retry on failure
- Don't lose uploaded files on error

### 5. Permission & Access
- Reps should only see their own projects' tasks
- Filter tasks by rep email/owner field
- Ops reviews all tasks in QuickBase

### 6. Real-time Updates
- Consider polling every 30-60 seconds for status updates
- Or use QuickBase webhooks if available
- Show "Updated X minutes ago" timestamp
- Refresh button for manual updates

### 7. Empty States
- Most projects won't have tasks
- Don't show task section if no tasks exist
- Clear messaging when all tasks complete

---

## Implementation Phases

### Phase 1: Read-Only View (Week 1)
**Goal:** Reps can see their tasks
- Query tasks from QuickBase
- Show task list on project detail page
- Display task status and submission history
- Show ops feedback

**Validation:** Rep can view all task info, but not yet submit

### Phase 2: Task Submission (Week 2)
**Goal:** Reps can upload and submit
- File upload UI component
- Create submission records via API
- Upload files to QuickBase
- Update task status

**Validation:** Rep uploads file → sees "Pending Review" → ops reviews in QuickBase → status updates in app

### Phase 3: Project List Integration (Week 3)
**Goal:** Tasks visible from projects view
- Add task badges to project cards
- Show task counts and progress
- Add filter for "projects with tasks"
- Sort by tasks needing attention

**Validation:** Rep sees which projects need work at a glance

### Phase 4: Notifications & Polish (Week 4)
**Goal:** Proactive alerts
- Notification when tasks assigned
- Alert when revision needed
- Celebrate when all approved
- Polish UI/UX based on feedback

**Validation:** Rep gets notified without checking app manually

---

## Technical Checklist

### Backend/API
- [ ] Create QuickBase service module/class
- [ ] Implement `getProjectTasks(projectId)`
- [ ] Implement `getRepProjects(repEmail)` with task counts
- [ ] Implement `createTaskSubmission(taskId, file, notes)`
- [ ] Implement file upload handler
- [ ] Add error handling and retries
- [ ] Add logging for debugging

### Frontend
- [ ] Create task data models/types
- [ ] Create `TaskSection` component
- [ ] Create `TaskCard` component
- [ ] Create file upload component with drag-drop
- [ ] Add task badges to project cards
- [ ] Add task filter to projects view
- [ ] Create notification system
- [ ] Add loading states and skeletons
- [ ] Add error states and retry buttons

### Testing
- [ ] Test with real QuickBase data
- [ ] Test file upload (various formats)
- [ ] Test submission → approval flow end-to-end
- [ ] Test revision → resubmission flow
- [ ] Test edge cases (network errors, large files, etc.)
- [ ] Test on mobile/responsive
- [ ] Get feedback from actual reps

### Documentation
- [ ] Document API endpoints
- [ ] Document component usage
- [ ] Create user guide for reps
- [ ] Document error codes/troubleshooting

---

## Key Points for Development Agent

1. **Data Already Exists:** 286 active task groups exist in QuickBase right now. This is a proven, working system.

2. **Read-Heavy:** Mostly reading data from QuickBase. Only write when rep submits.

3. **No Ops Changes:** Operations team keeps using QuickBase. We're only building the rep side.

4. **Project-Centric UX:** Tasks live within project context, not a separate section.

5. **Progressive Enhancement:** Start with read-only view, then add submission, then polish.

6. **Real QuickBase IDs:**
   - Task Groups table: `bu36gem4p`
   - Tasks table: `bu36ggiht`
   - Submissions table: `bu36g8j99`
   - Projects table: `br9kwm8na`

7. **File Upload is Critical:** This is the core action. Make it smooth and reliable.

8. **Status Tracking:** Three places to track status:
   - Task.Status (field 9) - "Not Started", "In Progress", "Complete"
   - Submission.Status (field 7) - "Pending Approval", "Reviewed"
   - Submission.Disposition (field 8) - "Approved" or "Needs Revision"

9. **Resubmissions:** Create NEW submission records, don't update existing ones.

10. **Visual Priority:** Use color coding and badges to make tasks that need attention obvious.

---

## Questions for Development Agent

Before starting implementation, please confirm:

1. **Framework:** What framework is the app built in? (React, Vue, React Native, etc.)
2. **State Management:** How do you handle state? (Redux, Context, Zustand, etc.)
3. **API Layer:** Do you have existing QuickBase integration or starting fresh?
4. **File Upload:** Do you have existing file upload components to reuse?
5. **Auth:** How do you identify the logged-in rep? (Email, user ID, etc.)
6. **Routing:** How are project detail pages routed?
7. **Styling:** What styling approach? (Tailwind, CSS modules, styled-components, etc.)
8. **Notifications:** Do you have a notification system already?

---

## Success Criteria

**MVP is successful when:**
- ✅ Rep can see rejected projects with task badges
- ✅ Rep can view list of tasks for a project
- ✅ Rep can upload files for each task
- ✅ Rep can see submission status (pending/approved/needs revision)
- ✅ Rep can resubmit if revision needed
- ✅ Rep can see when all tasks are complete
- ✅ File uploads successfully reach QuickBase
- ✅ Ops can review submissions in QuickBase without changes

**Bonus features:**
- Real-time status updates
- Notification system
- Task completion analytics
- Mobile-optimized experience
- Bulk upload for multiple tasks

---

## Reference Documents

See these files for technical details:
- `TASK_TABLES_DOCUMENTATION.md` - Complete table/field reference
- `TASK_WORKFLOW_INTEGRATION.md` - Detailed API examples
- `TASK_ANALYTICS_USE_CASES.md` - Query patterns and use cases

## Current State Evidence

**The system is LIVE and WORKING:**
- 286 active task groups in production
- 1,131 tasks created
- 349 submissions from reps
- Last activity: October 22, 2025 (yesterday)
- Used for intake rejection workflow
- Projects with tasks: 8899, 8982, 9095, 9104, 9119, etc.

**Example Real Task:**
- Project 9119: Had 3 tasks, all approved, now Active ✅
- Project 8982: Status "Rejected", has 3 tasks pending
- Tasks include: "Finance Stip Other", "Missing UB", "Utility Bill Rejected"

---

## Contact & Questions

If you need clarification on any part of this vision, ask about:
- UX flow and component behavior
- QuickBase data relationships
- API query patterns
- Edge case handling
- Testing approach

Let's build this! 🚀
