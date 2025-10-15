/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  typescript: {
    ignoreBuildErrors: false,
  },
  images: {
    domains: [],
  },
  async headers() {
    const isDev = process.env.NODE_ENV === 'development';

    return [
      {
        source: '/(.*)',
        headers: [
          {
            key: 'Content-Security-Policy',
            value: [
              "default-src 'self'",
              // Add 'unsafe-eval' in development for React hot reloading
              // Add chat-assets.frontapp.com for Front Chat widget
              isDev ? "script-src 'self' 'unsafe-inline' 'unsafe-eval' https://chat-assets.frontapp.com" : "script-src 'self' 'unsafe-inline' https://chat-assets.frontapp.com",
              // Add blob: for Front Chat dynamically generated styles
              "style-src 'self' 'unsafe-inline' blob:",
              "img-src 'self' data: https:",
              // Add chat-assets.frontapp.com for Front Chat fonts (Inter font)
              "font-src 'self' data: https://chat-assets.frontapp.com",
              // Add Front Chat domains: chat-api, chat.frontapp.com, regional chat servers (*.frontapp.com), Ably realtime (used by Front Chat), and bugsnag
              "connect-src 'self' https://kin.quickbase.com https://*.vercel.app https://*.frontapp.com https://*.ably.io https://*.ably-realtime.com https://sessions.bugsnag.com",
              "worker-src 'self' blob:",
              "frame-ancestors 'none'",
              "base-uri 'self'",
              "form-action 'self'",
            ].join('; '),
          },
          {
            key: 'X-Frame-Options',
            value: 'DENY',
          },
          {
            key: 'X-Content-Type-Options',
            value: 'nosniff',
          },
          {
            key: 'Referrer-Policy',
            value: 'origin-when-cross-origin',
          },
        ],
      },
    ]
  },
}

module.exports = nextConfig
