import { describe, it, expect } from 'vitest'
import fs from 'fs'
import path from 'path'

describe('Env/Public token exposure', () => {
  it('forbids NEXT_PUBLIC_*TOKEN* in env.example', () => {
    const p = path.join(process.cwd(), 'env.example')
    const content = fs.readFileSync(p, 'utf-8')
    expect(/NEXT_PUBLIC_.*TOKEN/i.test(content)).toBe(false)
  })
})


