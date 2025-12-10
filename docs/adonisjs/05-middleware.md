# Middleware

## Admin Auth Middleware

File: `app/Middleware/AdminAuth.ts`

\`\`\`typescript
import type { HttpContextContract } from '@ioc:Adonis/Core/HttpContext'

export default class AdminAuth {
  public async handle({ session, response }: HttpContextContract, next: () => Promise<void>) {
    const isAdmin = session.get('isAdmin')
    
    if (!isAdmin) {
      session.flash('error', 'Please login to access admin area')
      return response.redirect('/admin/login')
    }
    
    await next()
  }
}
\`\`\`

## Register Middleware

File: `start/kernel.ts`

Add the following to the named middleware section:

\`\`\`typescript
Server.middleware.registerNamed({
  adminAuth: () => import('App/Middleware/AdminAuth'),
})
