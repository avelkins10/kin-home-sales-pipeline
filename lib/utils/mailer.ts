import 'server-only'
import nodeFetch from 'node-fetch'

type MailParams = {
  to: string
  subject: string
  html: string
}

export async function sendMail(params: MailParams): Promise<void> {
  if (process.env.EMAIL_ENABLED !== 'true') return

  const provider = process.env.MAIL_PROVIDER || 'resend'

  if (provider === 'resend') {
    const key = process.env.RESEND_API_KEY
    if (!key) return
    await nodeFetch('https://api.resend.com/emails', {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${key}`,
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        from: process.env.MAIL_FROM || 'Kin Solar <no-reply@kinhome.com>',
        to: params.to,
        subject: params.subject,
        html: params.html
      })
    })
    return
  }

  if (provider === 'sendgrid') {
    const key = process.env.SENDGRID_API_KEY
    if (!key) return
    await nodeFetch('https://api.sendgrid.com/v3/mail/send', {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${key}`,
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        personalizations: [{ to: [{ email: params.to }] }],
        from: { email: (process.env.MAIL_FROM || 'no-reply@kinhome.com'), name: 'Kin Solar' },
        subject: params.subject,
        content: [{ type: 'text/html', value: params.html }]
      })
    })
    return
  }
}


