import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { NextRequest } from 'next/server';
import { POST } from '@/app/api/internal/audit/route';

// Mock the database client
const mockSql = vi.fn();
vi.mock('@/lib/db/client', () => ({
  sql: mockSql,
}));

// Mock the logger
vi.mock('@/lib/logging/logger', () => ({
  logError: vi.fn(),
  logInfo: vi.fn(),
}));

describe('Audit Logging API', () => {
  const originalEnv = process.env;

  beforeEach(() => {
    vi.resetAllMocks();
    process.env = { ...originalEnv };
    process.env.INTERNAL_API_SECRET = 'test-secret-123';
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  describe('Authentication', () => {
    it('should reject requests without secret header', async () => {
      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          action: 'test',
          resource: 'user',
          resourceId: '123',
          userId: 'user-123',
        }),
      });

      const response = await POST(request);
      expect(response.status).toBe(403);
      
      const body = await response.json();
      expect(body.error).toBe('Forbidden');
    });

    it('should reject requests with invalid secret', async () => {
      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': 'wrong-secret',
        },
        body: JSON.stringify({
          action: 'test',
          resource: 'user',
          resourceId: '123',
          userId: 'user-123',
        }),
      });

      const response = await POST(request);
      expect(response.status).toBe(403);
      
      const body = await response.json();
      expect(body.error).toBe('Forbidden');
    });

    it('should accept requests with valid secret', async () => {
      // Mock successful database response
      mockSql.mockResolvedValueOnce({
        rows: [{ name: 'Test User' }]
      });
      mockSql.mockResolvedValueOnce({});

      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': 'test-secret-123',
        },
        body: JSON.stringify({
          action: 'update',
          resource: 'user',
          resourceId: '123',
          userId: 'user-123',
          changes: {
            name: { old: 'Old Name', new: 'New Name' }
          },
        }),
      });

      const response = await POST(request);
      expect(response.status).toBe(200);
      
      const body = await response.json();
      expect(body.success).toBe(true);
    });
  });

  describe('Request Validation', () => {
    it('should reject invalid JSON', async () => {
      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': 'test-secret-123',
        },
        body: 'invalid json',
      });

      const response = await POST(request);
      expect(response.status).toBe(400);
      
      const body = await response.json();
      expect(body.error).toBe('Invalid JSON');
    });

    it('should reject requests missing required fields', async () => {
      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': 'test-secret-123',
        },
        body: JSON.stringify({
          action: 'test',
          // missing resource, resourceId, userId
        }),
      });

      const response = await POST(request);
      expect(response.status).toBe(400);
      
      const body = await response.json();
      expect(body.error).toBe('Validation failed');
      expect(body.details).toBeDefined();
    });

    it('should accept valid requests with optional fields', async () => {
      // Mock successful database response
      mockSql.mockResolvedValueOnce({
        rows: [{ name: 'Test User' }]
      });
      mockSql.mockResolvedValueOnce({});

      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': 'test-secret-123',
        },
        body: JSON.stringify({
          action: 'create',
          resource: 'project',
          resourceId: 'proj-456',
          userId: 'user-789',
          changes: {
            status: { old: null, new: 'Active' }
          },
          ipAddress: '192.168.1.1',
          userAgent: 'Mozilla/5.0...',
        }),
      });

      const response = await POST(request);
      expect(response.status).toBe(200);
      
      const body = await response.json();
      expect(body.success).toBe(true);
    });
  });

  describe('Database Operations', () => {
    it('should handle database errors gracefully', async () => {
      // Mock database error
      mockSql.mockRejectedValueOnce(new Error('Database connection failed'));

      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': 'test-secret-123',
        },
        body: JSON.stringify({
          action: 'delete',
          resource: 'office',
          resourceId: 'office-123',
          userId: 'user-456',
        }),
      });

      const response = await POST(request);
      expect(response.status).toBe(500);
      
      const body = await response.json();
      expect(body.error).toBe('Internal Server Error');
    });

    it('should insert audit log with correct data', async () => {
      // Mock successful database response
      mockSql.mockResolvedValueOnce({
        rows: [{ name: 'John Doe' }]
      });
      mockSql.mockResolvedValueOnce({});

      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': 'test-secret-123',
        },
        body: JSON.stringify({
          action: 'update',
          resource: 'user',
          resourceId: 'user-123',
          userId: 'admin-456',
          changes: {
            role: { old: 'closer', new: 'office_leader' }
          },
        }),
      });

      const response = await POST(request);
      expect(response.status).toBe(200);

      // Verify database calls
      expect(mockSql).toHaveBeenCalledTimes(2);
      
      // First call: get user name
      expect(mockSql).toHaveBeenNthCalledWith(1, 
        expect.stringContaining('SELECT name FROM users WHERE id =')
      );
      
      // Second call: insert audit log
      expect(mockSql).toHaveBeenNthCalledWith(2, 
        expect.stringContaining('INSERT INTO audit_logs')
      );
    });
  });

  describe('Environment Configuration', () => {
    it('should reject requests when INTERNAL_API_SECRET is not configured', async () => {
      delete process.env.INTERNAL_API_SECRET;

      const request = new NextRequest('http://localhost:3000/api/internal/audit', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': 'any-secret',
        },
        body: JSON.stringify({
          action: 'test',
          resource: 'user',
          resourceId: '123',
          userId: 'user-123',
        }),
      });

      const response = await POST(request);
      expect(response.status).toBe(403);
      
      const body = await response.json();
      expect(body.error).toBe('Forbidden');
    });
  });
});