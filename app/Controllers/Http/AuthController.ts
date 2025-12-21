import type { HttpContextContract } from '@ioc:Adonis/Core/HttpContext'
import Env from '@ioc:Adonis/Core/Env'

export default class AuthController {
  public async showLogin({ view }: HttpContextContract) {
    return view.render('admin/login')
  }

  public async login({ request, response, session }: HttpContextContract) {
    const { username, password } = request.only(['username', 'password'])
    
    const adminUsername = Env.get('ADMIN_USERNAME')
    const adminPassword = Env.get('ADMIN_PASSWORD')

    if (username === adminUsername && password === adminPassword) {
      session.put('isAdmin', true)
      session.flash('success', 'Login successful!')
      return response.redirect('/admin/dashboard')
    }

    session.flash('error', 'Invalid username or password')
    return response.redirect('/admin/login')
  }

  public async logout({ response, session }: HttpContextContract) {
    session.forget('isAdmin')
    session.flash('success', 'Logged out successfully!')
    return response.redirect('/admin/login')
  }
}