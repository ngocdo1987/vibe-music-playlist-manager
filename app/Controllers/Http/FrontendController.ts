import type { HttpContextContract } from '@ioc:Adonis/Core/HttpContext'
import Playlist from 'App/Models/Playlist'

export default class FrontendController {
  public async home({ view }: HttpContextContract) {
    const playlists = await Playlist.query()
      .withCount('songs')
      .orderBy('created_at', 'desc')
    
    return view.render('frontend/home', { playlists })
  }

  public async playlist({ params, view, response }: HttpContextContract) {
    const playlist = await Playlist.query()
      .where('slug', params.slug)
      .preload('songs', (query) => query.orderBy('position', 'asc'))
      .first()
    
    if (!playlist) {
      return response.status(404).send('Playlist not found')
    }
    
    return view.render('frontend/playlist', { playlist })
  }
}
