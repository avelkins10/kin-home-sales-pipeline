// lib/types/project.ts
export interface QuickbaseProject {
  [key: number]: {
    value: any;
  };
}

export interface Project {
  id: string;
  customerName: string;
  status: string;
  onHold: boolean;
  holdReason?: string;
  dateOnHold?: string;
  systemPrice: number;
  salesDate: string;
  closerId?: string;
  setterId?: string;
  grossPPW?: number;
  netPPW?: number;
  commissionablePPW?: number;
  installDate?: string;
  surveyDate?: string;
  designDate?: string;
  permitDate?: string;
  nemDate?: string;
  hoaDate?: string;
  inspectionDate?: string;
  ptoDate?: string;
  statusBar?: string;
}