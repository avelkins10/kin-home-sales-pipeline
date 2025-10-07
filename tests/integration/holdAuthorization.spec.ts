import { test, expect } from '@playwright/test'

test('returns 403 when user lacks access to project', async ({ request }) => {
  const res = await request.post('/api/projects/999999/hold', {
    data: { onHold: true }
  })
  expect([401, 403]).toContain(res.status())
})


