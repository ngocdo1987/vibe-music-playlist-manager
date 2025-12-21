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