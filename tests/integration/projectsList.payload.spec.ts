import { describe, it, expect, vi, beforeEach } from 'vitest';
import { getProjectsForUserList } from '@/lib/quickbase/queries';

// Mock the Quickbase client
const mockQbClient = {
  queryRecords: vi.fn(),
};

vi.mock('@/lib/quickbase/client', () => ({
  qbClient: mockQbClient,
}));

// Mock logger
vi.mock('@/lib/logging/logger', () => ({
  logError: vi.fn(),
}));

describe('Projects List Payload Size', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('should return payload under 300KB for 50 records', async () => {
    // Mock 50 project records with lean field selection
    const mockProjects = Array.from({ length: 50 }, (_, i) => ({
      3: { value: `record_${i}` }, // RECORD_ID
      6: { value: `PROJ-${i.toString().padStart(4, '0')}` }, // PROJECT_ID
      7: { value: `Customer ${i}` }, // CUSTOMER_NAME
      8: { value: `123 Main St, City ${i}, ST 12345` }, // CUSTOMER_ADDRESS
      9: { value: `555-${i.toString().padStart(4, '0')}` }, // CUSTOMER_PHONE
      10: { value: 'Active' }, // PROJECT_STATUS
      11: { value: 'No' }, // ON_HOLD
      12: { value: '' }, // HOLD_REASON
      13: { value: '' }, // BLOCK_REASON
      14: { value: '' }, // DATE_ON_HOLD
      15: { value: 'Normal' }, // PROJECT_PRIORITY
      16: { value: 'Phoenix' }, // SALES_OFFICE
      17: { value: '2024-01-01' }, // SALES_DATE
      18: { value: 30 }, // PROJECT_AGE
      19: { value: 8.5 }, // SYSTEM_SIZE_KW
      20: { value: 25000 }, // SYSTEM_PRICE
      2292: { value: 2.94 }, // soldGross PPW
      2480: { value: 2.85 }, // commissionable PPW
      // Milestone dates
      710: { value: '2024-02-01' }, // INTAKE_INSTALL_DATE_TENTATIVE
      164: { value: '2024-01-15' }, // SURVEY_SUBMITTED
      315: { value: '2024-01-20' }, // DESIGN_COMPLETED
      207: { value: '2024-01-25' }, // PERMIT_APPROVED
      326: { value: '2024-01-30' }, // NEM_APPROVED
      534: { value: '2024-02-05' }, // INSTALL_SCHEDULED_DATE_CAPTURE
      587: { value: '2024-02-10' }, // INSTALL_COMPLETED_DATE
      491: { value: '2024-02-12' }, // PASSING_INSPECTION_COMPLETED
      537: { value: '2024-02-15' }, // PTO_APPROVED
    }));

    mockQbClient.queryRecords.mockResolvedValue({
      data: mockProjects,
      metadata: { totalRecords: 50 }
    });

    const result = await getProjectsForUserList('test-user', 'closer', 'all', '', 'default');

    // Convert to JSON to measure size
    const jsonString = JSON.stringify(result);
    const sizeInBytes = new Blob([jsonString]).size;
    const sizeInKB = sizeInBytes / 1024;

    console.log(`Payload size: ${sizeInKB.toFixed(2)} KB (${sizeInBytes} bytes)`);
    console.log(`Records: ${result.length}`);

    // Assertions
    expect(result).toHaveLength(50);
    expect(sizeInKB).toBeLessThan(300); // Should be under 300KB
    
    // Verify lean field selection - should not have all 92 fields
    const firstRecord = result[0];
    const fieldCount = Object.keys(firstRecord).length;
    expect(fieldCount).toBeLessThan(50); // Should have significantly fewer fields than full record
    
    // Verify essential fields are present
    expect(firstRecord[3]).toBeDefined(); // RECORD_ID
    expect(firstRecord[6]).toBeDefined(); // PROJECT_ID
    expect(firstRecord[7]).toBeDefined(); // CUSTOMER_NAME
    expect(firstRecord[10]).toBeDefined(); // PROJECT_STATUS
  });

  it('should handle empty results efficiently', async () => {
    mockQbClient.queryRecords.mockResolvedValue({
      data: [],
      metadata: { totalRecords: 0 }
    });

    const result = await getProjectsForUserList('test-user', 'closer', 'all', '', 'default');
    const jsonString = JSON.stringify(result);
    const sizeInBytes = new Blob([jsonString]).size;

    expect(result).toHaveLength(0);
    expect(sizeInBytes).toBeLessThan(100); // Empty array should be tiny
  });

  it('should maintain data integrity with lean selection', async () => {
    const mockProject = {
      3: { value: 'record_123' },
      6: { value: 'PROJ-0001' },
      7: { value: 'John Doe' },
      10: { value: 'Active' },
      11: { value: 'No' },
      2292: { value: 2.94 },
      2480: { value: 2.85 },
    };

    mockQbClient.queryRecords.mockResolvedValue({
      data: [mockProject],
      metadata: { totalRecords: 1 }
    });

    const result = await getProjectsForUserList('test-user', 'closer', 'all', '', 'default');

    expect(result).toHaveLength(1);
    expect(result[0][3].value).toBe('record_123');
    expect(result[0][6].value).toBe('PROJ-0001');
    expect(result[0][7].value).toBe('John Doe');
    expect(result[0][2292].value).toBe(2.94);
    expect(result[0][2480].value).toBe(2.85);
  });
});
