export interface Office {
  id: string;
  name: string;
  isActive: boolean;
  region: string;
  leaderId: string;
  leaderName: string;
  /** Count of managers with office-based roles assigned to this office */
  managerCount: number;
  /** Total count of all users assigned to this office */
  userCount: number;
  activeProjects: number;
  createdAt: string;
  updatedAt: string;
  /** All managers assigned to this office via office_assignments table */
  assignedManagers?: Array<{
    userId: string;
    userName: string;
    userRole: string;
    accessLevel: 'view' | 'manage' | 'admin';
  }>;
}

export interface CreateOfficeInput {
  name: string;
  region: string;
  leaderId: string;
}

export interface UpdateOfficeInput {
  name?: string;
  region?: string;
  leaderId?: string;
  isActive?: boolean;
}

export type Region = 'southwest' | 'southeast' | 'midwest' | 'northeast' | 'west';
