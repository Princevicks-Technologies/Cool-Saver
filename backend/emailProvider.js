import fetch from 'node-fetch';

export function createEmailProvider(env) {
  if (env.EMAIL_PROVIDER === 'sendgrid') return sendgrid(env);
  if (env.EMAIL_PROVIDER === 'mailgun') return mailgun(env);
  if (env.EMAIL_PROVIDER === 'ses') return ses(env);
  return consoleProvider();
}

function consoleProvider() {
  return {
    async send({ to, subject, text }) {
      console.log(`[EMAIL:console] to=${to} subject=${subject} text=${text}`);
    }
  };
}

function sendgrid(env) {
  return {
    async send({ to, subject, text }) {
      await fetch('https://api.sendgrid.com/v3/mail/send', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${env.SENDGRID_API_KEY}`,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          personalizations: [{ to: [{ email: to }] }],
          from: { email: env.EMAIL_FROM },
          subject,
          content: [{ type: 'text/plain', value: text }]
        })
      });
    }
  };
}

function mailgun(env) {
  return {
    async send({ to, subject, text }) {
      console.log('Mailgun adapter ready. Configure domain + endpoint in production.', to, subject, text);
    }
  };
}

function ses(env) {
  return {
    async send({ to, subject, text }) {
      console.log(`SES adapter stub send (${env.SES_REGION}):`, to, subject, text);
    }
  };
}
