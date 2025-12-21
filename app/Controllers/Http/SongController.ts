import type { HttpContextContract } from '@ioc:Adonis/Core/HttpContext'
import Song from 'App/Models/Song'
import Application from '@ioc:Adonis/Core/Application'
import fs from 'fs/promises'

export default class SongController {
  public async destroy({ params, response, session }: HttpContextContract) {
    const song = await Song.findOrFail(params.id)
    
    // Delete the MP3 file
    try {
      await fs.unlink(Application.publicPath(`mp3/${song.filename}`))
    } catch (error) {
      console.error(`Failed to delete file: ${song.filename}`)
    }
    
    const playlistId = song.playlistId
    await song.delete()
    
    // Reorder remaining songs
    const remainingSongs = await Song.query()
      .where('playlist_id', playlistId)
      .orderBy('position', 'asc')
    
    for (let i = 0; i < remainingSongs.length; i++) {
      remainingSongs[i].position = i
      await remainingSongs[i].save()
    }
    
    session.flash('success', 'Song deleted successfully!')
    return response.redirect().back()
  }

  public async updateTitle({ params, request, response }: HttpContextContract) {
    const song = await Song.findOrFail(params.id)
    const { title } = request.only(['title'])
    
    song.title = title
    await song.save()
    
    return response.json({ success: true })
  }
}