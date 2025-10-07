# PWA Icons

This directory contains icons for the Progressive Web App (PWA) functionality.

## Required Icons

- `icon-192x192.png` - Standard Android icon
- `icon-512x512.png` - High-res Android icon
- `icon-180x180.png` - iOS home screen icon
- `favicon.ico` - Browser favicon (optional)

## Generating Icons

### Option 1: Using an online tool
1. Visit https://realfavicongenerator.net/
2. Upload a square logo (at least 512x512px)
3. Configure settings for iOS, Android, and web
4. Download and extract to this directory

### Option 2: Using ImageMagick (command line)
```bash
# From a 512x512 source image
convert logo.png -resize 192x192 icon-192x192.png
convert logo.png -resize 512x512 icon-512x512.png
convert logo.png -resize 180x180 icon-180x180.png
```

### Option 3: Manual creation
1. Create a square logo in your design tool (Figma, Sketch, etc.)
2. Export at 512x512px as PNG
3. Use an image editor to resize to required dimensions
4. Ensure icons have transparent backgrounds or solid color backgrounds

## Design Guidelines

- Use the Kin Home brand colors (blue #3b82f6)
- Keep design simple and recognizable at small sizes
- Ensure good contrast for visibility
- Test on both light and dark backgrounds
- Consider using a rounded square or circle shape

## Testing

After adding icons:
1. Run `npm run dev`
2. Open Chrome DevTools → Application → Manifest
3. Verify all icons are loaded correctly
4. Test 'Add to Home Screen' on iPad/Android device

**Note:** Until icons are created, the PWA will use default browser icons. The app will still function but won't have custom branding when installed.
