/**
 * RepCard API Type Definitions
 *
 * Comprehensive types for RepCard API resources and responses
 */

export interface RepCardUser {
  id: number;
  firstName: string;
  lastName: string;
  email: string;
  phone?: string;
  companyId: number;
  officeId?: number;
  officeName?: string;
  role?: string;
  status: number; // 1 = active, 0 = inactive
  profileImage?: string;
  createdAt: string;
  updatedAt: string;
  // Performance metrics (if included in details endpoint)
  totalSales?: number;
  totalRevenue?: number;
  totalAppointments?: number;
  firstVerifiedDoorKnock?: string;
  firstAppointment?: string;
}

export interface RepCardUserMinimal {
  id: number;
  firstName: string;
  lastName: string;
  email: string;
  companyId: number;
  officeId?: number;
}

export interface RepCardOffice {
  id: number;
  name: string;
  companyId: number;
  address?: string;
  city?: string;
  state?: string;
  zipCode?: string;
  phone?: string;
  managerUserId?: number;
  createdAt: string;
  updatedAt: string;
}

export interface RepCardCompany {
  id: number;
  name: string;
  domain?: string;
  logo?: string;
  createdAt: string;
  updatedAt: string;
}

export interface RepCardCustomer {
  id: number;
  firstName: string;
  lastName: string;
  email?: string;
  phone?: string;
  address?: string;
  city?: string;
  state?: string;
  zipCode?: string;
  assignedUserId?: number;
  statusId?: number;
  companyId: number;
  createdAt: string;
  updatedAt: string;
  // Custom fields for solar industry
  customFields?: RepCardCustomerCustomFields;
}

export interface RepCardCustomerCustomFields {
  systemSizeKW?: number;
  systemCost?: number;
  financier?: string;
  offset?: number;
  installDate?: string;
  permitStatus?: string;
  utilityCompany?: string;
  [key: string]: any; // Allow additional custom fields
}

export interface RepCardCustomerStatus {
  id: number;
  statusName: string;
  shortName: string;
  type: number;
  userId?: number;
  companyId: number;
  statusOrder: number;
  colour: string;
  shape: number;
  isGlobal: number;
  iconName: string;
  isBaseStatus: number;
  iconUrl?: string;
  statusShape: string;
  createdAt: string;
  updatedAt: string;
}

export interface RepCardCustomerNote {
  id: number;
  customerId: number;
  userId: number;
  note: string;
  createdAt: string;
  updatedAt: string;
  user?: RepCardUserMinimal;
}

export interface RepCardAppointment {
  id: number;
  title: string;
  appointmentLink: string;
  notes: string;
  userId: number; // setter user ID
  setter: {
    id: number;
    fullName: string;
    email: string;
    phoneNumber: string;
  };
  closerId: number;
  closer: {
    id: number;
    fullName: string;
    email: string;
    phoneNumber: string;
  };
  calendarId: number;
  contact: {
    id: number;
    fullName: string;
    email: string;
    phoneNumber: string;
    contactSource: string;
  };
  startAt: string; // datetime
  startAtTimezone: string;
  endAtTimezone: string;
  endAt: string; // datetime
  appointmentLocation: string;
  status: {
    id: number;
    title: string;
    category: {
      id: number;
      title: string;
      categoryColour: string;
    };
  } | null;
  createdAt: string; // ISO timestamp
  updatedAt: string; // ISO timestamp
  durationTime: number; // minutes
}

export interface RepCardCalendarEvent {
  id: number;
  userId: number;
  title: string;
  description?: string;
  startDate: string;
  endDate: string;
  allDay: boolean;
  location?: string;
  attendees?: number[];
  createdAt: string;
  updatedAt: string;
}

export interface RepCardLeaderboardEntry {
  userId: number;
  userName: string;
  officeId?: number;
  officeName?: string;
  rank: number;
  metric: number; // Sales count, revenue, appointments, etc.
  metricType: 'sales' | 'revenue' | 'appointments' | 'doors_knocked';
  period: string;
}

export interface RepCardCustomerAttachment {
  id: number;
  attachmentUrl: string;
  userId: number; // user who uploaded
  customerId: number;
}

export interface RepCardAppointmentAttachment {
  id: number;
  attachmentUrl: string;
  type: string; // e.g., "image"
  userId: number; // user who uploaded
  appointmentId: number;
}

// API Response wrappers
export interface RepCardApiResponse<T> {
  status: number;
  statusCode: number;
  message: string;
  result: T;
}

export interface RepCardPaginatedResponse<T> {
  status: number;
  statusCode: number;
  message: string;
  result: {
    data: T[];
    total: number;
    perPage: number;
    currentPage: number;
    lastPage: number;
  };
}

// Query parameter types
export interface RepCardUserQueryParams {
  companyId?: number;
  officeId?: number;
  perPage?: number;
  page?: number;
  search?: string;
  status?: 0 | 1; // 0 = inactive, 1 = active
  role?: string;
  firstVerifiedDoorKnock?: 0 | 1;
  firstAppointment?: 0 | 1;
}

export interface RepCardCustomerQueryParams {
  companyId?: number;
  officeId?: number;
  userId?: number; // Assigned user
  perPage?: number;
  page?: number;
  search?: string;
  statusId?: number;
  startDate?: string; // YYYY-MM-DD
  endDate?: string; // YYYY-MM-DD
}

export interface RepCardLeaderboardQueryParams {
  companyId?: number;
  officeId?: number;
  startDate?: string; // YYYY-MM-DD
  endDate?: string; // YYYY-MM-DD
  metric?: 'sales' | 'revenue' | 'appointments' | 'doors_knocked';
  perPage?: number;
  page?: number;
}

// Customer Status Log Types
export interface RepCardStatusLogStatus {
  id: number;
  statusName: string;
  colour: string;
  iconUrl?: string;
}

export interface RepCardCustomerStatusLog {
  _id: string; // MongoDB ID
  customerId: number;
  customer: {
    id: number;
    firstName: string;
    lastName: string;
    email: string;
    countryCode: string;
    phoneNumber: string;
  } | null;
  statusFrom: RepCardStatusLogStatus | null; // previous status, null for first status
  statusTo: RepCardStatusLogStatus; // new status
  userId: number; // user who made the change
  companyId: number;
  createdAt: string; // ISO timestamp
  updatedAt: string; // ISO timestamp
  user: {
    id: number;
    firstName: string;
    lastName: string;
    email: string;
    username?: string;
    office: string;
    team: string | null;
    jobTitle?: string;
    phoneNumber?: string;
    image?: string;
    rating: string | number;
    bio?: string;
    companyName?: string;
    badgeId?: string;
    qrCode?: string;
  };
}

export interface RepCardStatusLogQueryParams {
  fromDate?: string; // YYYY-MM-DD
  toDate?: string; // YYYY-MM-DD
  userId?: string; // single user ID
  customerId?: string; // single customer ID
  statusId?: string; // single status ID
  // Keep plural forms for backward compatibility if API supports both
  userIds?: string; // comma-separated IDs
  customerIds?: string; // comma-separated IDs
  statusIds?: string; // comma-separated IDs
  perPage?: number;
  page?: number;
}

export interface RepCardStatusLogResponse {
  status: number;
  statusCode: number;
  message: string;
  result: {
    data: RepCardCustomerStatusLog[];
    totalCount: number;
    currentPage: number;
    totalPages: number;
  };
}

// Enhanced Appointment Query Parameters
export interface RepCardAppointmentQueryParams {
  fromDate?: string; // YYYY-MM-DD
  toDate?: string; // YYYY-MM-DD
  customerIds?: string; // comma-separated IDs
  statusIds?: string; // comma-separated IDs
  setterIds?: string; // comma-separated IDs
  closerIds?: string; // comma-separated IDs
  perPage?: number;
  page?: number;
}

export interface RepCardAppointmentResponse {
  status: number;
  statusCode: number;
  message: string;
  result: {
    data: RepCardAppointment[];
    totalCount: number;
    currentPage: number;
    totalPages: number;
  };
}

// Attachment Query Parameters
export interface RepCardCustomerAttachmentQueryParams {
  perPage?: number;
  page?: number;
  userIds?: string; // comma-separated IDs
  fromDate?: string; // YYYY-MM-DD
  toDate?: string; // YYYY-MM-DD
  customerIds?: string; // comma-separated IDs
}

export interface RepCardAppointmentAttachmentQueryParams {
  perPage?: number;
  page?: number;
  setterIds?: string; // comma-separated IDs
  closerIds?: string; // comma-separated IDs
  fromDate?: string; // YYYY-MM-DD
  toDate?: string; // YYYY-MM-DD
  customerIds?: string; // comma-separated IDs
  statusIds?: string; // comma-separated IDs
}

export interface RepCardAttachmentResponse<T> {
  status: number;
  statusCode: number;
  message: string;
  result: {
    data: T[];
    totalCount: number;
    currentPage: number;
    totalPages: number;
  };
}

// Sync-related types for our dashboard
export interface RepCardUserSyncResult {
  userId: string; // Our dashboard user ID
  repcardId: number;
  email: string;
  name: string;
  officeId?: number;
  officeName?: string;
  syncMethod: 'email_match' | 'manual' | 'quickbase_closer_id';
  confidence: number; // 0.0 to 1.0
  syncedAt: string;
  notes?: string;
}

// API Response Types for RepCard Integration Endpoints

// User Stats Response Types
export interface UserVolumeStats {
  doorsKnocked: number;
  appointmentsSet: number;
  salesClosed: number;
  revenueGenerated: number;
}

export interface UserQualityStats {
  appointmentSpeed: { percentage: number; averageHours: number };
  attachmentRate: { percentage: number; totalAttachments: number };
  rescheduleRate: { average: number; totalReschedules: number };
  followUpConsistency: { percentage: number; totalFollowUps: number };
}

export interface UserEfficiencyStats {
  doorsPerAppointment: number;
  appointmentsPerSale: number;
  averageDealSize: number;
  averageTimeToClose: number;
}

export interface UserStatsResponse {
  user: { 
    id: string; 
    name: string; 
    email: string; 
    office: string; 
    role: string; 
    repcardUserId: string 
  };
  volumeStats: UserVolumeStats;
  qualityStats: UserQualityStats;
  efficiencyStats: UserEfficiencyStats;
  metadata: { 
    startDate: string; 
    endDate: string; 
    timeRange?: string; 
    cached: boolean; 
    calculatedAt: string 
  };
}

// Leaderboard Response Types
export type LeaderboardMetric = 'doors_knocked' | 'appointments_set' | 'sales_closed' | 'revenue' | 'quality_score' | 'appointment_speed' | 'attachment_rate';
export type LeaderboardRole = 'setter' | 'closer' | 'all';

export interface LeaderboardEntry {
  rank: number;
  userId: string;
  userName: string;
  userEmail: string;
  office: string;
  role: string;
  metricValue: number;
  metricType: string;
  trend?: 'up' | 'down' | 'same' | 'new';
}

export interface LeaderboardResponse {
  leaderboard: LeaderboardEntry[];
  metadata: { 
    role: string; 
    metric: string; 
    timeRange: string; 
    startDate: string; 
    endDate: string; 
    officeIds?: string[]; 
    totalEntries: number; 
    page: number; 
    limit: number; 
    totalPages: number; 
    cached: boolean; 
    calculatedAt: string 
  };
}

// Office Stats Response Types
export interface OfficeVolumeStats {
  totalDoorsKnocked: number;
  totalAppointmentsSet: number;
  totalSalesClosed: number;
  totalRevenue: number;
}

export interface OfficeQualityStats {
  appointmentSpeed: { percentage: number; averageHours: number };
  attachmentRate: { percentage: number };
  rescheduleRate: { average: number };
  followUpConsistency: { percentage: number };
  compositeScore: number;
}

export interface OfficeAveragePerRep {
  doorsKnocked: number;
  appointmentsSet: number;
  salesClosed: number;
  revenue: number;
}

export interface OfficeTopPerformer {
  userId: string;
  userName: string;
  qualityScore: number;
}

export interface OfficeStatsResponse {
  officeName: string;
  teamSize: number;
  volumeStats: OfficeVolumeStats;
  qualityStats: OfficeQualityStats;
  averagePerRep: OfficeAveragePerRep;
  topPerformers: OfficeTopPerformer[];
  metadata: { 
    startDate: string; 
    endDate: string; 
    timeRange?: string; 
    cached: boolean; 
    calculatedAt: string 
  };
}

// Error Response Types
export interface RepCardApiError {
  error: string;
  message?: string;
  stack?: string;
}

export interface GracefulDegradationResponse {
  hasRepcardData: false;
  message: string;
  userId?: string;
  userName?: string;
  userEmail?: string;
  officeName?: string;
}
