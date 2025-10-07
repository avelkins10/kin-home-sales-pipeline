// lib/types/settings.ts

export interface SystemSettings {
  quickbaseRealm: string;
  quickbaseToken: string;
  milestoneSLA: {
    survey: number;
    design: number;
    permit: number;
    nem: number;
    install: number;
    inspection: number;
    pto: number;
  };
  warningThreshold: number;
  criticalThreshold: number;
  holdReasons: string[];
  dateFormat: 'MM/DD/YYYY' | 'DD/MM/YYYY' | 'YYYY-MM-DD';
  timezone: string;
  sessionTimeout: number;
}

export type UpdateSystemSettingsInput = Partial<SystemSettings>;

export interface TestConnectionInput {
  realm: string;
  token: string;
}


