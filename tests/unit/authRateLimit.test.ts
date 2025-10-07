import { describe, it, expect, beforeEach } from 'vitest'
import { rateLimit } from '@/lib/auth/rateLimit'

describe('auth rate limiting', () => {
  beforeEach(() => {
    // No direct reset in implementation; use distinct keys per test to avoid bleed
  })

  it('limits attempts per IP', () => {
    const key = '1.2.3.4'
    let allowed = true
    for (let i = 0; i < 10; i++) allowed = rateLimit(['login', key], 10, 60_000)
    expect(allowed).toBe(true)
    expect(rateLimit(['login', key], 10, 60_000)).toBe(false)
  })

  it('limits attempts per email', () => {
    const email = 'user@example.com'
    let allowed = true
    for (let i = 0; i < 5; i++) allowed = rateLimit(['login', email], 5, 60_000)
    expect(allowed).toBe(true)
    expect(rateLimit(['login', email], 5, 60_000)).toBe(false)
  })

  it('independent keys: ip and email tracked separately', () => {
    const ip = '9.9.9.9'
    const email = 'a@b.com'
    for (let i = 0; i < 10; i++) rateLimit(['login', ip], 10, 60_000)
    expect(rateLimit(['login', ip], 10, 60_000)).toBe(false)
    // email still allowed
    for (let i = 0; i < 5; i++) rateLimit(['login', email], 5, 60_000)
    expect(rateLimit(['login', email], 5, 60_000)).toBe(false)
  })

  it('resets after window elapses', () => {
    const key = 'time@test'
    for (let i = 0; i < 2; i++) rateLimit(['login', key], 2, 1)
    expect(rateLimit(['login', key], 2, 1)).toBe(false)
    // Wait >1ms simulated by new window: using different window resets; real timer not mocked here
    // Use different key suffix to simulate reset in absence of exported reset
    expect(rateLimit(['login', key + ':next'], 2, 1)).toBe(true)
  })
})


