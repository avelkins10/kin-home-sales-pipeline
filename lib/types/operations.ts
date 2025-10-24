/**
 * Operations-specific type definitions
 * These types define the data structures for the Operations app
 */

export interface WorkOrder {
  id: string;
  projectId: string;
  title: string;
  description: string;
  status: WorkOrderStatus;
  priority: WorkOrderPriority;
  assignedTo: string;
  assignedCrew: string;
  scheduledDate: Date;
  completedDate?: Date;
  createdAt: Date;
  updatedAt: Date;
}

export type WorkOrderStatus = 
  | 'pending'
  | 'scheduled'
  | 'in_progress'
  | 'completed'
  | 'cancelled'
  | 'on_hold';

export type WorkOrderPriority = 
  | 'low'
  | 'medium'
  | 'high'
  | 'urgent';

export interface InventoryItem {
  id: string;
  name: string;
  description: string;
  category: string;
  sku: string;
  quantity: number;
  minQuantity: number;
  maxQuantity: number;
  unit: string;
  cost: number;
  supplier: string;
  location: string;
  lastUpdated: Date;
  createdAt: Date;
}

export interface CrewSchedule {
  id: string;
  crewId: string;
  crewName: string;
  workOrderId: string;
  scheduledDate: Date;
  startTime: string;
  endTime: string;
  status: CrewScheduleStatus;
  notes?: string;
  createdAt: Date;
  updatedAt: Date;
}

export type CrewScheduleStatus = 
  | 'scheduled'
  | 'in_progress'
  | 'completed'
  | 'cancelled'
  | 'delayed';

export interface QualityCheck {
  id: string;
  workOrderId: string;
  projectId: string;
  checkType: QualityCheckType;
  status: QualityCheckStatus;
  performedBy: string;
  performedAt: Date;
  notes?: string;
  photos?: string[];
  createdAt: Date;
  updatedAt: Date;
}

export type QualityCheckType = 
  | 'installation'
  | 'safety'
  | 'electrical'
  | 'structural'
  | 'final_inspection';

export type QualityCheckStatus = 
  | 'pending'
  | 'passed'
  | 'failed'
  | 'needs_rework';

export interface OperationsMetrics {
  activeWorkOrders: number;
  completedWorkOrders: number;
  crewsScheduled: number;
  inventoryItems: number;
  qualityPassRate: number;
  averageCompletionTime: number;
  onTimeCompletionRate: number;
}

export interface OperationsDashboardData {
  metrics: OperationsMetrics;
  recentWorkOrders: WorkOrder[];
  upcomingSchedules: CrewSchedule[];
  inventoryAlerts: InventoryItem[];
  qualityChecks: QualityCheck[];
}
