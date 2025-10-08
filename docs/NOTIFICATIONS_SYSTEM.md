# Notifications System

## Overview

A unified notification system for the Kin Home Sales Pipeline app that provides real-time updates for Quickbase notes, internal team messages, and system alerts. Built with a Linear-inspired UI and 30-second polling for real-time updates.

## Features

- **Three Priority Levels**: Critical (red), Normal (blue), Info (gray)
- **Multiple Notification Types**: Quickbase notes, internal messages, system alerts
- **Real-time Updates**: 30-second polling (upgradeable to Server-Sent Events)
- **Smart Read Tracking**: Automatic mark-as-read on view
- **Global Bell Icon**: Shows total unread count with critical alert pulsing
- **Project Badges**: Unread notification counts on each project row
- **Dropdown Center**: Filterable notification panel with priority tabs
- **Cross-device Sync**: Postgres-backed for consistent state

## Installation

### 1. Run Database Migration

```bash
npm run migrate:notifications
```

This will:
- Create the `notifications` table
- Set up performance indexes
- Create the `update_updated_at_column` trigger function
- Verify all components are properly created

### 2. Verify Environment

Ensure your `.env.local` has:
```env
DATABASE_URL=your_vercel_postgres_url
```

### 3. Start Development Server

```bash
npm run dev
```

## Architecture

### Database Schema

**Table**: `notifications`

| Column | Type | Description |
|--------|------|-------------|
| `id` | SERIAL | Primary key |
| `user_id` | VARCHAR(255) | User email or ID |
| `project_id` | INTEGER | Related project record ID |
| `type` | VARCHAR(50) | 'quickbase_note', 'internal_message', 'system_alert' |
| `priority` | VARCHAR(20) | 'critical', 'normal', 'info' |
| `source` | VARCHAR(50) | 'quickbase', 'internal', 'system' |
| `title` | VARCHAR(255) | Notification title |
| `message` | TEXT | Optional message body |
| `metadata` | JSONB | Flexible type-specific data |
| `sender_id` | VARCHAR(255) | Sender identifier (optional) |
| `sender_name` | VARCHAR(255) | Sender display name (optional) |
| `sender_role` | VARCHAR(50) | Sender role (optional) |
| `icon` | VARCHAR(50) | Icon name (default: 'bell') |
| `color` | VARCHAR(20) | Color scheme (default: 'blue') |
| `action_url` | VARCHAR(500) | Navigation target URL |
| `is_read` | BOOLEAN | Read status (default: false) |
| `read_at` | TIMESTAMPTZ | When marked as read |
| `created_at` | TIMESTAMPTZ | Creation timestamp |
| `updated_at` | TIMESTAMPTZ | Auto-updated timestamp |

**Indexes**:
- `idx_notifications_user_unread`: Fast unread queries
- `idx_notifications_project`: Project-specific lookups
- `idx_notifications_user_project`: Combined filtering
- `idx_notifications_type`: Type-based queries

### File Structure

```
lib/
  types/notification.ts           # TypeScript type definitions
  db/notifications.ts             # Database functions (server-only)
  constants/notificationStyles.ts # Design tokens and styles
  hooks/useNotifications.ts       # Client-side React Query hooks

components/
  notifications/
    NotificationBell.tsx          # Global bell icon with dropdown
    NotificationCenter.tsx        # Dropdown panel with filters
    NotificationCard.tsx          # Single notification display
  ui/
    UnreadBadge.tsx              # Reusable unread count badge
  layout/
    DashboardHeader.tsx          # Header with bell integration
  projects/
    NotesSection.tsx             # Modified to mark notifications read
    ProjectRow.tsx               # Modified to show unread badges

app/api/
  notifications/
    route.ts                     # GET (list), POST (create)
    [id]/read/route.ts          # POST (mark single as read)
    unread-counts/route.ts      # GET (unread counts)

scripts/
  migrations/
    001-create-notifications.sql # Database schema
  run-notifications-migration.js # Migration runner
```

## API Endpoints

### GET `/api/notifications`

Fetch notifications for current user.

**Query Parameters**:
- `limit`: Number of results (1-100, default: 50)
- `offset`: Pagination offset (default: 0)
- `unreadOnly`: Filter to unread only (default: false)
- `projectId`: Filter by project ID (optional)

**Response**:
```json
{
  "notifications": [...],
  "count": 15,
  "has_more": true
}
```

### POST `/api/notifications`

Create a new notification.

**Body**:
```json
{
  "user_id": "user@example.com",
  "project_id": 123,
  "type": "internal_message",
  "priority": "normal",
  "source": "internal",
  "title": "New message from operations",
  "message": "System is ready for review",
  "action_url": "/projects/123"
}
```

### POST `/api/notifications/[id]/read`

Mark a single notification as read.

**Response**:
```json
{
  "success": true,
  "notification": {...}
}
```

### GET `/api/notifications/unread-counts`

Get unread counts for current user.

**Response**:
```json
{
  "total": 42,
  "by_priority": {
    "critical": 3,
    "normal": 30,
    "info": 9
  },
  "by_project": {
    "123": 5,
    "456": 2
  }
}
```

## React Hooks

### `useNotifications(options)`

Fetch and poll for notifications.

```tsx
const { data, isLoading } = useNotifications({
  limit: 50,
  offset: 0,
  unreadOnly: true,
  projectId: 123,
  enablePolling: true, // 30-second polling
});
```

### `useUnreadCounts(enablePolling)`

Fetch total and categorized unread counts.

```tsx
const { data } = useUnreadCounts(true);
// data.total, data.by_priority, data.by_project
```

### `useMarkAsRead()`

Mark notification as read.

```tsx
const markAsRead = useMarkAsRead();
await markAsRead.mutateAsync(notificationId);
```

### `useProjectUnreadCount(projectId)`

Get unread count for specific project.

```tsx
const unreadCount = useProjectUnreadCount(123);
```

### `useCriticalUnreadCount()`

Get count of critical unread notifications.

```tsx
const criticalCount = useCriticalUnreadCount();
```

## Components

### NotificationBell

Global bell icon with dropdown. Auto-included in dashboard header.

**Features**:
- Shows total unread count badge
- Critical notifications pulse in red
- Click to open NotificationCenter dropdown
- Auto-closes on outside click or Escape key

### NotificationCenter

Dropdown panel showing all notifications.

**Features**:
- Filter by priority (all, critical, normal, info)
- Toggle unread-only mode
- Click notification to navigate and mark as read
- Empty state when no notifications

### NotificationCard

Individual notification display.

**Features**:
- Color-coded left border by priority
- Icon based on notification type
- Relative timestamps (e.g., "2h ago")
- Sender information
- Unread indicator dot
- Auto-marks as read on click
- Navigates to action_url if provided

### UnreadBadge

Reusable badge component.

**Props**:
- `count`: Number to display
- `variant`: 'default' | 'critical'
- `size`: 'small' | 'large'

```tsx
<UnreadBadge count={5} variant="critical" size="small" />
```

## Design System

### Priority Colors

| Priority | Border | Background | Text | Use Case |
|----------|--------|------------|------|----------|
| Critical | Red | Red-50 | Red-900 | Ops notes, urgent issues |
| Normal | Blue | Blue-50 | Blue-900 | Team messages, updates |
| Info | Slate | Slate-50 | Slate-900 | System notifications |

### Typography

- **Title**: `text-sm font-semibold` (bold if unread)
- **Message**: `text-sm text-slate-700`
- **Metadata**: `text-xs text-slate-500`
- **Timestamp**: `text-xs text-slate-500`

### Animations

- Card enter: `fade-in slide-in-from-top-1 duration-200`
- Card hover: `hover:shadow-md hover:border-indigo-200`
- Badge: `fade-in zoom-in duration-200`

## Usage Examples

### Create Notification Programmatically

```typescript
// In an API route or server action
import { createNotification } from '@/lib/db/notifications';

const notification = await createNotification({
  user_id: 'user@example.com',
  project_id: 123,
  type: 'quickbase_note',
  priority: 'critical',
  source: 'quickbase',
  title: 'New operations note',
  message: 'Project requires immediate attention',
  sender_name: 'Operations Team',
  action_url: '/projects/123',
  metadata: {
    note_id: 456,
    category: 'Acceptance',
  },
});
```

### Display Project-Specific Notifications

```tsx
import { useProjectUnreadCount } from '@/lib/hooks/useNotifications';
import { UnreadBadge } from '@/components/ui/UnreadBadge';

function ProjectHeader({ projectId }) {
  const unreadCount = useProjectUnreadCount(projectId);

  return (
    <div className="flex items-center gap-2">
      <h1>Project Details</h1>
      {unreadCount > 0 && (
        <UnreadBadge count={unreadCount} />
      )}
    </div>
  );
}
```

### Mark All Project Notifications as Read

```tsx
import { markAllNotificationsAsRead } from '@/lib/db/notifications';

// On project detail page load (server-side)
await markAllNotificationsAsRead(userId, projectId);
```

## Future Enhancements

### Phase 2 - Real-time Updates
- Replace polling with Server-Sent Events (SSE)
- WebSocket support for instant notifications
- Push notifications for mobile

### Phase 3 - Advanced Features
- Notification preferences per user
- Mute/snooze capabilities
- Mark all as read bulk action
- Archive old notifications
- Search and filter history

### Phase 4 - Internal Messaging
- Reply to notifications inline
- Thread-based conversations
- @mentions support
- File attachments

## Troubleshooting

### Notifications not appearing

1. Verify database migration ran successfully:
```bash
npm run migrate:notifications
```

2. Check database connection:
```bash
# Verify DATABASE_URL is set
echo $DATABASE_URL
```

3. Inspect browser console for errors

### Counts not updating

1. Check polling is enabled in hooks
2. Verify 30-second interval is appropriate for your use case
3. Clear React Query cache:
```tsx
queryClient.invalidateQueries({ queryKey: ['notifications'] });
```

### Read status not syncing

1. Verify API route is being called (Network tab)
2. Check user authentication
3. Ensure notification belongs to current user

## Performance

### Current Metrics
- Database query time: <50ms (with indexes)
- API response time: <100ms
- Polling interval: 30 seconds
- Client cache: 20 seconds stale time

### Optimization Tips

1. **Pagination**: Use `limit` and `offset` for large result sets
2. **Filtering**: Use `unreadOnly` to reduce payload size
3. **Caching**: React Query handles deduplication automatically
4. **Indexes**: All critical queries use indexed columns

## Security

- All endpoints require authentication via `requireAuth()`
- Users can only access their own notifications
- SQL injection prevented via parameterized queries
- XSS protection via React's default escaping

## Support

For issues or questions:
1. Check this documentation
2. Review API endpoint logs
3. Inspect browser console
4. Contact development team
