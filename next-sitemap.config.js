module.exports = {
  siteUrl: process.env.NEXT_PUBLIC_APP_URL || 'https://your-production-domain.vercel.app',
  generateRobotsTxt: true,
  robotsTxtOptions: {
    policies: [
      { userAgent: '*', allow: '/' },
      { userAgent: '*', disallow: '/api/' }
    ]
  },
  exclude: ['/api/*', '/login'],
  generateIndexSitemap: false
}
