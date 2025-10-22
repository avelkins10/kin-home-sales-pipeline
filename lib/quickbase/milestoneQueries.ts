// lib/quickbase/milestoneQueries.ts
export async function getMilestoneConfig(): Promise<any> {
  // In a real app, fetch this dynamically or load from a more robust config source.
  // For demonstration, returning a simplified structure.
  return {
    milestones: [
      { id: 'intake', name: 'Intake', fieldId: 461 },
      { id: 'survey', name: 'Survey', fieldId: 164 },
      { id: 'design', name: 'Design', fieldId: 315 },
      { id: 'permitting', name: 'Permitting', fieldId: 207 },
      { id: 'nem', name: 'NEM', fieldId: 326 },
      { id: 'install', name: 'Installation', fieldId: 710 },
      { id: 'inspection', name: 'Inspection', fieldId: 491 },
      { id: 'pto', name: 'PTO', fieldId: 537 },
    ],
    // Add any other relevant milestone data
  };
}
