export interface Office {
  id: string;
  name: string;
  region: string;
  leaderId: string;
  leaderName: string;
  userCount: number;
  activeProjects: number;
  createdAt: string;
  updatedAt: string;
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
}

export type Region = 'southwest' | 'southeast' | 'midwest' | 'northeast' | 'west';
