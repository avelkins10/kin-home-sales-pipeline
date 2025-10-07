import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { NextRequest } from 'next/server';
import { GET } from '@/app/api/projects/route';
import { QuickbaseProject } from '@/lib/types/project';

// Mock the Quickbase client
vi.mock('@/lib/quickbase/client', () => ({
  qbClient: {
    query: vi.fn(),
  },
}));

// Mock the auth guards
vi.mock('@/lib/auth/guards', () => ({
  requireAuth: vi.fn(),
}));

// Mock the cache
vi.mock('@/lib/cache/projectsCache', () => ({
  getCachedProjects: vi.fn(() => null),
  setCachedProjects: vi.fn(),
  cacheStats: {
    hits: 0,
    misses: 0,
    evictions: 0,
    expiredRemovals: 0,
    currentSize: 0,
  },
}));

// Mock the queries module
vi.mock('@/lib/quickbase/queries', () => ({
  getProjectsForUserList: vi.fn(),
}));

describe('Projects List Payload Regression Test', () => {
  const mockAuth = {
    authorized: true,
    session: {
      user: {
        quickbaseUserId: 'test-user-123',
        role: 'closer',
        salesOffice: ['Office A'],
      },
    },
  };

  const mockProjects: QuickbaseProject[] = Array.from({ length: 50 }, (_, i) => ({
    [3]: { value: i + 1 }, // recordId
    [PROJECT_FIELDS.CUSTOMER_NAME]: { value: `Customer ${i + 1}` },
    [PROJECT_FIELDS.CUSTOMER_ADDRESS]: { value: `${i + 1} Main St, City, State` },
    [PROJECT_FIELDS.CUSTOMER_PHONE]: { value: `555-${String(i + 1).padStart(4, '0')}` },
    [PROJECT_FIELDS.PROJECT_STATUS]: { value: i % 3 === 0 ? 'On Hold - Customer' : 'Active' },
    [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: (5 + (i % 10)).toString() },
    [PROJECT_FIELDS.SYSTEM_PRICE]: { value: (25000 + (i * 1000)).toString() },
    [PROJECT_FIELDS.SALES_DATE]: { value: new Date(Date.now() - (i * 24 * 60 * 60 * 1000)).toISOString() },
    [PROJECT_FIELDS.CLOSER_NAME]: { value: `Closer ${i + 1}` },
    [PROJECT_FIELDS.SETTER_NAME]: { value: `Setter ${i + 1}` },
    [PROJECT_FIELDS.ON_HOLD]: { value: i % 3 === 0 ? 'Yes' : 'No' },
    [PROJECT_FIELDS.HOLD_REASON]: { value: i % 3 === 0 ? 'Customer requested hold' : '' },
    [PROJECT_FIELDS.BLOCK_REASON]: { value: '' },
    [PROJECT_FIELDS.DATE_ON_HOLD]: { value: i % 3 === 0 ? new Date().toISOString() : '' },
    [2292]: { value: (3.5 + (i % 2)).toString() }, // soldGross PPW
    [2480]: { value: (3.2 + (i % 2)).toString() }, // commissionable PPW
  }));

  beforeEach(() => {
    vi.clearAllMocks();
    
    // Mock requireAuth to return authorized user
    const { requireAuth } = await import('@/lib/auth/guards');
    vi.mocked(requireAuth).mockResolvedValue(mockAuth);
    
    // Mock getProjectsForUserList to return mock data
    const { getProjectsForUserList } = await import('@/lib/quickbase/queries');
    vi.mocked(getProjectsForUserList).mockResolvedValue(mockProjects);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('should return projects list with payload under 300KB', async () => {
    const request = new NextRequest('http://localhost:3000/api/projects');
    const response = await GET(request);
    
    expect(response.status).toBe(200);
    
    const responseText = await response.text();
    const responseSize = new Blob([responseText]).size;
    
    // Assert payload size is under 300KB
    expect(responseSize).toBeLessThan(300 * 1024);
    
    const projects = JSON.parse(responseText);
    expect(Array.isArray(projects)).toBe(true);
    expect(projects).toHaveLength(50);
  });

  it('should include essential fields for ProjectRow rendering', async () => {
    const request = new NextRequest('http://localhost:3000/api/projects');
    const response = await GET(request);
    
    expect(response.status).toBe(200);
    
    const projects = await response.json();
    const firstProject = projects[0];
    
    // Essential fields for ProjectRow component
    expect(firstProject[3]).toBeDefined(); // recordId
    expect(firstProject[PROJECT_FIELDS.CUSTOMER_NAME]).toBeDefined(); // customerName
    expect(firstProject[PROJECT_FIELDS.CUSTOMER_ADDRESS]).toBeDefined(); // customerAddress
    expect(firstProject[PROJECT_FIELDS.CUSTOMER_PHONE]).toBeDefined(); // customerPhone
    expect(firstProject[PROJECT_FIELDS.PROJECT_STATUS]).toBeDefined(); // projectStatus
    expect(firstProject[PROJECT_FIELDS.SYSTEM_SIZE_KW]).toBeDefined(); // systemSizeKw
    expect(firstProject[PROJECT_FIELDS.SYSTEM_PRICE]).toBeDefined(); // systemPrice
    expect(firstProject[PROJECT_FIELDS.SALES_DATE]).toBeDefined(); // salesDate
    expect(firstProject[PROJECT_FIELDS.CLOSER_NAME]).toBeDefined(); // closerName
    expect(firstProject[PROJECT_FIELDS.SETTER_NAME]).toBeDefined(); // setterName
    expect(firstProject[PROJECT_FIELDS.ON_HOLD]).toBeDefined(); // onHold
    expect(firstProject[PROJECT_FIELDS.HOLD_REASON]).toBeDefined(); // holdReason
    expect(firstProject[2292]).toBeDefined(); // soldGross PPW
    expect(firstProject[2480]).toBeDefined(); // commissionable PPW
  });

  it('should include hold status fields for hold badge rendering', async () => {
    const request = new NextRequest('http://localhost:3000/api/projects');
    const response = await GET(request);
    
    expect(response.status).toBe(200);
    
    const projects = await response.json();
    
    // Check that some projects have hold status
    const holdProjects = projects.filter((p: QuickbaseProject) => 
      p[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes'
    );
    expect(holdProjects.length).toBeGreaterThan(0);
    
    // Check hold reason is present for hold projects
    holdProjects.forEach((project: QuickbaseProject) => {
      expect(project[PROJECT_FIELDS.HOLD_REASON]?.value).toBeDefined();
    });
  });

  it('should include PPW fields for PPW display', async () => {
    const request = new NextRequest('http://localhost:3000/api/projects');
    const response = await GET(request);
    
    expect(response.status).toBe(200);
    
    const projects = await response.json();
    const firstProject = projects[0];
    
    // PPW fields should be present and numeric
    expect(firstProject[2292]?.value).toBeDefined(); // soldGross
    expect(firstProject[2480]?.value).toBeDefined(); // commissionable
    expect(typeof parseFloat(firstProject[2292]?.value || '0')).toBe('number');
    expect(typeof parseFloat(firstProject[2480]?.value || '0')).toBe('number');
  });

  it('should include age calculation fields', async () => {
    const request = new NextRequest('http://localhost:3000/api/projects');
    const response = await GET(request);
    
    expect(response.status).toBe(200);
    
    const projects = await response.json();
    const firstProject = projects[0];
    
    // Sales date should be present for age calculation
    expect(firstProject[PROJECT_FIELDS.SALES_DATE]?.value).toBeDefined();
    expect(firstProject[PROJECT_FIELDS.SALES_DATE]?.value).toMatch(/^\d{4}-\d{2}-\d{2}T/);
  });

  it('should handle large payload efficiently', async () => {
    // Create a larger dataset to test payload size limits
    const largeMockProjects: QuickbaseProject[] = Array.from({ length: 100 }, (_, i) => ({
      [3]: { value: i + 1 },
      [PROJECT_FIELDS.CUSTOMER_NAME]: { value: `Customer ${i + 1}` },
      [PROJECT_FIELDS.CUSTOMER_ADDRESS]: { value: `${i + 1} Main St, City, State, ZIP Code` },
      [PROJECT_FIELDS.CUSTOMER_PHONE]: { value: `555-${String(i + 1).padStart(4, '0')}` },
      [PROJECT_FIELDS.PROJECT_STATUS]: { value: i % 3 === 0 ? 'On Hold - Customer' : 'Active' },
      [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: (5 + (i % 10)).toString() },
      [PROJECT_FIELDS.SYSTEM_PRICE]: { value: (25000 + (i * 1000)).toString() },
      [PROJECT_FIELDS.SALES_DATE]: { value: new Date(Date.now() - (i * 24 * 60 * 60 * 1000)).toISOString() },
      [PROJECT_FIELDS.CLOSER_NAME]: { value: `Closer ${i + 1}` },
      [PROJECT_FIELDS.SETTER_NAME]: { value: `Setter ${i + 1}` },
      [PROJECT_FIELDS.ON_HOLD]: { value: i % 3 === 0 ? 'Yes' : 'No' },
      [PROJECT_FIELDS.HOLD_REASON]: { value: i % 3 === 0 ? 'Customer requested hold' : '' },
      [PROJECT_FIELDS.BLOCK_REASON]: { value: '' },
      [PROJECT_FIELDS.DATE_ON_HOLD]: { value: i % 3 === 0 ? new Date().toISOString() : '' },
      [2292]: { value: (3.5 + (i % 2)).toString() },
      [2480]: { value: (3.2 + (i % 2)).toString() },
    }));

    const { getProjectsForUserList } = await import('@/lib/quickbase/queries');
    vi.mocked(getProjectsForUserList).mockResolvedValue(largeMockProjects);

    const request = new NextRequest('http://localhost:3000/api/projects');
    const response = await GET(request);
    
    expect(response.status).toBe(200);
    
    const responseText = await response.text();
    const responseSize = new Blob([responseText]).size;
    
    // Even with 100 projects, payload should be under 300KB
    expect(responseSize).toBeLessThan(300 * 1024);
    
    const projects = JSON.parse(responseText);
    expect(projects).toHaveLength(100);
  });
});

// Import PROJECT_FIELDS for the test
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';