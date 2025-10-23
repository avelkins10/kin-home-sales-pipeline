// lib/quickbase/client.js - JavaScript version for Node.js scripts

// Use global fetch (available in Node 18+)

class QuickbaseClient {
  constructor() {
    this.realm = process.env.QUICKBASE_REALM || '';
    this.token = process.env.QUICKBASE_TOKEN || '';
    this.requestQueue = [];
    this.isProcessing = false;
    this.requestsThisSecond = 0;
    this.lastRequestTime = 0;

    // Quickbase rate limit: 10 requests/second
    this.MAX_REQUESTS_PER_SECOND = 10;
    this.REQUEST_WINDOW_MS = 1000;

    if (!this.realm || !this.token) {
      console.warn('Quickbase credentials not configured');
    }
  }

  async waitForRateLimit() {
    const now = Date.now();
    const timeSinceLastRequest = now - this.lastRequestTime;

    // Reset counter if we're in a new second
    if (timeSinceLastRequest >= this.REQUEST_WINDOW_MS) {
      this.requestsThisSecond = 0;
      this.lastRequestTime = now;
      return;
    }

    // If we've hit the limit, wait until the next second
    if (this.requestsThisSecond >= this.MAX_REQUESTS_PER_SECOND) {
      const waitTime = this.REQUEST_WINDOW_MS - timeSinceLastRequest;
      await new Promise(resolve => setTimeout(resolve, waitTime));
      this.requestsThisSecond = 0;
      this.lastRequestTime = Date.now();
    }
  }

  async processQueue() {
    if (this.isProcessing || this.requestQueue.length === 0) {
      return;
    }

    this.isProcessing = true;

    while (this.requestQueue.length > 0) {
      await this.waitForRateLimit();

      const item = this.requestQueue.shift();
      if (!item) continue;

      try {
        const result = await item.request();
        item.resolve(result);
        this.requestsThisSecond++;
      } catch (error) {
        item.reject(error);
      }
    }

    this.isProcessing = false;
  }

  queueRequest(request) {
    return new Promise((resolve, reject) => {
      this.requestQueue.push({
        request,
        resolve,
        reject,
        timestamp: Date.now(),
      });
      this.processQueue();
    });
  }

  async queryRecords(params) {
    return this.queueRequest(async () => {
      const response = await fetch('https://api.quickbase.com/v1/records/query', {
        method: 'POST',
        headers: {
          'QB-Realm-Hostname': this.realm,
          'Authorization': `QB-USER-TOKEN ${this.token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(params),
      });

      if (!response.ok) {
        let errorDetails = {
          status: response.status,
          statusText: response.statusText
        };

        try {
          const error = await response.json();
          errorDetails.error = error;
          const errorMessage = error.message || response.statusText;
          console.error('[QB Client] Query failed:', JSON.stringify(errorDetails, null, 2));
          console.error('[QB Client] Request params:', JSON.stringify(params, null, 2));
          throw new Error(`Quickbase API error: ${errorMessage} - ${JSON.stringify(error)}`);
        } catch (parseError) {
          console.error('[QB Client] Failed to parse error response:', errorDetails);
          throw new Error(`Quickbase API error: ${response.statusText}`);
        }
      }

      const json = await response.json();
      return json;
    });
  }

  async updateRecord(params) {
    return this.queueRequest(async () => {
      const response = await fetch('https://api.quickbase.com/v1/records', {
        method: 'POST',
        headers: {
          'QB-Realm-Hostname': this.realm,
          'Authorization': `QB-USER-TOKEN ${this.token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(params),
      });

      if (!response.ok) {
        const error = await response.json();
        const errorMessage = error.message || response.statusText;
        throw new Error(`Quickbase API error: ${errorMessage}`);
      }

      const json = await response.json();
      return json;
    });
  }

  async getFieldInfo(tableId, fieldId) {
    return this.queueRequest(async () => {
      const response = await fetch(
        `https://api.quickbase.com/v1/fields/${fieldId}?tableId=${tableId}`,
        {
          method: 'GET',
          headers: {
            'QB-Realm-Hostname': this.realm,
            'Authorization': `QB-USER-TOKEN ${this.token}`,
          },
        }
      );

      if (!response.ok) {
        const errorMessage = `Failed to get field info: ${response.statusText}`;
        throw new Error(errorMessage);
      }

      const json = await response.json();
      return json;
    });
  }
}

module.exports = { qbClient: new QuickbaseClient() };
