# Mobile Responsiveness Standards

## Breakpoints (Tailwind Config)
- **xs**: 475px (phone)
- **mobile**: 640px (large phone)
- **ipad**: 768px (tablet portrait)
- **ipad-lg**: 1024px (tablet landscape)
- **desktop**: 1280px (desktop)

## Touch Target Sizes
- **Minimum**: 44px × 44px (iOS/Android standard)
- **Buttons**: h-8 w-8 (32px) + p-2 (8px padding) = 48px ✓
- **Navigation items**: px-3 py-2 = 44px height ✓
- **Icon buttons**: h-11 w-11 (44px) for standalone icons
- **Checkboxes**: h-4 w-4 wrapped in clickable div with padding

## Responsive Patterns
- **Grids**: grid-cols-1 md:grid-cols-2 lg:grid-cols-3
- **Flex layouts**: flex-col mobile:flex-row
- **Text sizes**: text-sm mobile:text-base ipad:text-lg
- **Padding**: p-3 mobile:p-4 ipad:p-6
- **Gaps**: gap-3 mobile:gap-4 ipad:gap-6

## Mobile-First Components
- **TopNavbar**: Mobile menu with hamburger icon
- **PCCalendar**: Agenda view on mobile, month view on desktop
- **ProjectDetailModal**: Full screen on mobile, max-w-6xl on desktop
- **Tables**: Horizontal scroll with ScrollArea component
- **Charts**: ResponsiveContainer from Recharts (100% width)

## Testing Checklist
- [ ] Test on iPhone SE (375px width)
- [ ] Test on iPhone 12/13 (390px width)
- [ ] Test on iPad (768px width)
- [ ] Test on iPad Pro (1024px width)
- [ ] Verify all touch targets are 44px minimum
- [ ] Verify text is readable (min 14px)
- [ ] Test navigation flow
- [ ] Test modals and dialogs
- [ ] Test forms and inputs
- [ ] Test charts and visualizations
- [ ] Test horizontal scrolling (tables, calendars)
- [ ] Test keyboard handling on mobile

## Known Issues
- None currently identified

## Future Enhancements
- Add swipe gestures for calendar navigation
- Add pull-to-refresh on mobile
- Add card view for tables on mobile
- Add bottom sheet for modals on mobile
