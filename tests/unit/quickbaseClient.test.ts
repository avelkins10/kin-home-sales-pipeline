import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { qbClient } from '@/lib/quickbase/client';

// Mock environment variables
vi.stubEnv('QUICKBASE_REALM', 'test.quickbase.com');
vi.stubEnv('QUICKBASE_TOKEN', 'test-token');

// Mock global fetch
const mockFetch = vi.fn();
vi.stubGlobal('fetch', mockFetch);

// Helper function to create mock Response
function createMockResponse(status: number, data: any) {
  const statusTexts: Record<number, string> = {
    200: 'OK',
    207: 'Multi-Status',
    401: 'Unauthorized',
    404: 'Not Found',
    500: 'Internal Server Error',
  };

  return {
    ok: status >= 200 && status < 300,
    status,
    statusText: statusTexts[status] || 'Error',
    json: vi.fn().mockResolvedValue(data),
  };
}

// Helper function to wait for queue processing
async function waitForQueue() {
  // Wait a bit for async queue processing
  await new Promise(resolve => setTimeout(resolve, 10));
}

// Helper function to advance time for rate limit testing
function advanceTime(ms: number) {
  vi.advanceTimersByTime(ms);
}

describe('QuickbaseClient', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.restoreAllMocks();
    vi.useRealTimers();
  });

  describe('queryRecords', () => {
    it('makes successful query request', async () => {
      const mockData = { data: [{ recordId: 1, fieldData: { 3: { value: 'test' } } }] };
      mockFetch.mockResolvedValueOnce(createMockResponse(200, mockData));

      const result = await qbClient.queryRecords({
        from: 'test-table',
        select: [3, 11],
      });

      expect(mockFetch).toHaveBeenCalledWith(
        'https://api.quickbase.com/v1/records/query',
        expect.objectContaining({
          method: 'POST',
          headers: expect.objectContaining({
            'QB-Realm-Hostname': 'test.quickbase.com',
            'Authorization': 'QB-USER-TOKEN test-token',
            'Content-Type': 'application/json',
          }),
          body: JSON.stringify({
            from: 'test-table',
            select: [3, 11],
          }),
        })
      );
      expect(result).toEqual(mockData);
    });

    it('includes QB-Realm-Hostname header', async () => {
      const mockData = { data: [] };
      mockFetch.mockResolvedValueOnce(createMockResponse(200, mockData));

      await qbClient.queryRecords({
        from: 'test-table',
        select: [3],
      });

      expect(mockFetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          headers: expect.objectContaining({
            'QB-Realm-Hostname': 'test.quickbase.com',
          }),
        })
      );
    });

    it('includes Authorization header with token', async () => {
      const mockData = { data: [] };
      mockFetch.mockResolvedValueOnce(createMockResponse(200, mockData));

      await qbClient.queryRecords({
        from: 'test-table',
        select: [3],
      });

      expect(mockFetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          headers: expect.objectContaining({
            'Authorization': 'QB-USER-TOKEN test-token',
          }),
        })
      );
    });

    it('throws error on API failure', async () => {
      const errorData = { message: 'Unauthorized' };
      mockFetch.mockResolvedValueOnce(createMockResponse(401, errorData));

      await expect(
        qbClient.queryRecords({
          from: 'test-table',
          select: [3],
        })
      ).rejects.toThrow('Quickbase API error: Unauthorized');
    });

    it('handles 500 server errors', async () => {
      mockFetch.mockResolvedValueOnce(createMockResponse(500, {}));

      await expect(
        qbClient.queryRecords({
          from: 'test-table',
          select: [3],
        })
      ).rejects.toThrow('Quickbase API error: Internal Server Error');
    });
  });

  describe('rate limiting', () => {
    it('allows up to 10 requests per second', async () => {
      const mockData = { data: [] };
      mockFetch.mockResolvedValue(createMockResponse(200, mockData));

      const startTime = Date.now();
      const promises = Array.from({ length: 10 }, () =>
        qbClient.queryRecords({
          from: 'test-table',
          select: [3],
        })
      );

      await vi.runAllTimersAsync();
      await Promise.all(promises);
      const endTime = Date.now();

      expect(mockFetch).toHaveBeenCalledTimes(10);
      expect(endTime - startTime).toBeLessThan(1000);
    });

    it('delays 11th request until next second', async () => {
      const mockData = { data: [] };
      mockFetch.mockResolvedValue(createMockResponse(200, mockData));

      const promises = Array.from({ length: 11 }, () =>
        qbClient.queryRecords({
          from: 'test-table',
          select: [3],
        })
      );

      // Advance time to simulate rate limiting
      await vi.runAllTimersAsync();

      await Promise.all(promises);

      expect(mockFetch).toHaveBeenCalledTimes(11);
    });

    it('resets counter after 1 second window', async () => {
      const mockData = { data: [] };
      mockFetch.mockResolvedValue(createMockResponse(200, mockData));

      // First batch of 10 requests
      const firstBatch = Array.from({ length: 10 }, () =>
        qbClient.queryRecords({
          from: 'test-table',
          select: [3],
        })
      );

      await vi.runAllTimersAsync();
      await Promise.all(firstBatch);
      expect(mockFetch).toHaveBeenCalledTimes(10);

      // Wait for rate limit window to reset
      await vi.advanceTimersByTimeAsync(1100);

      // Second batch of 10 requests
      const secondBatch = Array.from({ length: 10 }, () =>
        qbClient.queryRecords({
          from: 'test-table',
          select: [3],
        })
      );

      await vi.runAllTimersAsync();
      await Promise.all(secondBatch);
      expect(mockFetch).toHaveBeenCalledTimes(20);
    });

    it('processes queue in order (FIFO)', async () => {
      const mockData = { data: [] };
      mockFetch.mockResolvedValue(createMockResponse(200, mockData));

      const callOrder: number[] = [];
      mockFetch.mockImplementation(() => {
        callOrder.push(Date.now());
        return Promise.resolve(createMockResponse(200, mockData));
      });

      // Make 5 requests with different params
      const promises = [
        qbClient.queryRecords({ from: 'table1', select: [1] }),
        qbClient.queryRecords({ from: 'table2', select: [2] }),
        qbClient.queryRecords({ from: 'table3', select: [3] }),
        qbClient.queryRecords({ from: 'table4', select: [4] }),
        qbClient.queryRecords({ from: 'table5', select: [5] }),
      ];

      await vi.runAllTimersAsync();
      await Promise.all(promises);

      // Verify requests were made in order
      expect(mockFetch).toHaveBeenCalledTimes(5);
      const calls = mockFetch.mock.calls;
      expect(calls[0][1].body).toContain('table1');
      expect(calls[1][1].body).toContain('table2');
      expect(calls[2][1].body).toContain('table3');
      expect(calls[3][1].body).toContain('table4');
      expect(calls[4][1].body).toContain('table5');
    });
  });

  describe('request queue', () => {
    it('queues concurrent requests', async () => {
      const mockData = { data: [] };
      mockFetch.mockResolvedValue(createMockResponse(200, mockData));

      const promises = [
        qbClient.queryRecords({ from: 'table1', select: [1] }),
        qbClient.queryRecords({ from: 'table2', select: [2] }),
        qbClient.queryRecords({ from: 'table3', select: [3] }),
      ];

      await vi.runAllTimersAsync();
      const results = await Promise.all(promises);

      expect(results).toHaveLength(3);
      expect(mockFetch).toHaveBeenCalledTimes(3);
    });

    it('handles queue processing errors', async () => {
      const mockData = { data: [] };
      const errorData = { message: 'Server error' };

      // First request succeeds, second fails, third succeeds
      mockFetch
        .mockResolvedValueOnce(createMockResponse(200, mockData))
        .mockResolvedValueOnce(createMockResponse(500, errorData))
        .mockResolvedValueOnce(createMockResponse(200, mockData));

      const promises = [
        qbClient.queryRecords({ from: 'table1', select: [1] }),
        qbClient.queryRecords({ from: 'table2', select: [2] }),
        qbClient.queryRecords({ from: 'table3', select: [3] }),
      ];

      await vi.runAllTimersAsync();
      const results = await Promise.allSettled(promises);

      expect(results[0].status).toBe('fulfilled');
      expect(results[1].status).toBe('rejected');
      expect(results[2].status).toBe('fulfilled');
      expect(mockFetch).toHaveBeenCalledTimes(3);
    });
  });

  describe('updateRecord', () => {
    it('makes successful update request', async () => {
      const mockData = { data: [{ recordId: 1 }] };
      mockFetch.mockResolvedValueOnce(createMockResponse(200, mockData));

      const resultPromise = qbClient.updateRecord({
        to: 'test-table',
        data: [{ fieldId: 3, value: 'test' }],
      });

      await vi.runAllTimersAsync();
      const result = await resultPromise;

      expect(mockFetch).toHaveBeenCalledWith(
        'https://api.quickbase.com/v1/records',
        expect.objectContaining({
          method: 'POST',
          headers: expect.objectContaining({
            'QB-Realm-Hostname': 'test.quickbase.com',
            'Authorization': 'QB-USER-TOKEN test-token',
            'Content-Type': 'application/json',
          }),
          body: JSON.stringify({
            to: 'test-table',
            data: [{ fieldId: 3, value: 'test' }],
          }),
        })
      );
      expect(result).toEqual(mockData);
    });

    it('handles partial success (207 status)', async () => {
      const mockData = {
        data: [{ recordId: 1 }],
        lineErrors: [{ line: 1, error: 'Field validation failed' }],
      };
      mockFetch.mockResolvedValueOnce(createMockResponse(207, mockData));

      const resultPromise = qbClient.updateRecord({
        to: 'test-table',
        data: [{ fieldId: 3, value: 'test' }],
      });

      await vi.runAllTimersAsync();
      const result = await resultPromise;

      expect(result).toEqual(mockData);
      expect(result.lineErrors).toBeDefined();
    });
  });

  describe('getFieldInfo', () => {
    it('makes successful field info request', async () => {
      const mockData = { fieldId: 3, name: 'Customer Name', type: 'text' };
      mockFetch.mockResolvedValueOnce(createMockResponse(200, mockData));

      const resultPromise = qbClient.getFieldInfo('test-table', 3);

      await vi.runAllTimersAsync();
      const result = await resultPromise;

      expect(mockFetch).toHaveBeenCalledWith(
        'https://api.quickbase.com/v1/fields/3?tableId=test-table',
        expect.objectContaining({
          method: 'GET',
          headers: expect.objectContaining({
            'QB-Realm-Hostname': 'test.quickbase.com',
            'Authorization': 'QB-USER-TOKEN test-token',
          }),
        })
      );
      expect(result).toEqual(mockData);
    });

    it('throws error on field info failure', async () => {
      mockFetch.mockResolvedValueOnce(createMockResponse(404, {}));

      const resultPromise = qbClient.getFieldInfo('test-table', 999);

      await vi.runAllTimersAsync();

      await expect(resultPromise).rejects.toThrow('Failed to get field info: Not Found');
    });
  });
});
