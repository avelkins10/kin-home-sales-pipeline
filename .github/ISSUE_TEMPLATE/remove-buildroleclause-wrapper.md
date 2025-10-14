---
name: Remove buildRoleClause wrapper function
about: Remove the deprecated buildRoleClause wrapper after one release cycle
title: "[CLEANUP] Remove buildRoleClause wrapper function"
labels: ["cleanup", "deprecated", "breaking-change"]
assignees: []
---

## Summary
Remove the deprecated `buildRoleClause()` wrapper function from `lib/quickbase/queries.ts` after one release cycle.

## Background
The `buildRoleClause()` function was deprecated in favor of using `buildProjectAccessClause()` directly from `@/lib/auth/projectAuthorization`. This wrapper was kept for backward compatibility but should be removed after one release cycle to prevent future drift.

## Changes Required

### 1. Remove the wrapper function
- Delete the `buildRoleClause()` function from `lib/quickbase/queries.ts`
- Remove the deprecation warning logic

### 2. Update any remaining references
- Search for any remaining calls to `buildRoleClause()` in the codebase
- Replace with direct calls to `buildProjectAccessClause()`

### 3. Update tests
- Remove or update any tests that specifically test the wrapper function
- Ensure all tests use the direct function

### 4. Update documentation
- Remove any references to `buildRoleClause()` from documentation
- Update any examples to use `buildProjectAccessClause()` directly

## Files to Modify
- `lib/quickbase/queries.ts` - Remove the wrapper function
- Any files that still reference `buildRoleClause()` (should be none after current updates)
- Test files that test the wrapper function

## Verification
- [ ] All tests pass
- [ ] No references to `buildRoleClause()` remain in the codebase
- [ ] Documentation is updated
- [ ] No breaking changes for external consumers (if any)

## Notes
- This is a breaking change for any external code that might be using the wrapper
- The wrapper was added for backward compatibility and should be safe to remove after one release cycle
- All internal references have been updated to use the direct function
