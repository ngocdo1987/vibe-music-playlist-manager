# Controllers

## Auth Controller

File: `app/Controllers/Http/AuthController.ts`

\`\`\`typescript
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
\`\`\`

## Playlist Controller

File: `app/Controllers/Http/PlaylistController.ts`

\`\`\`typescript
import type { HttpContextContract } from '@ioc:Adonis/Core/HttpContext'
import Playlist from 'App/Models/Playlist'
import Song from 'App/Models/Song'
import Application from '@ioc:Adonis/Core/Application'
import { v4 as uuidv4 } from 'uuid'
import fs from 'fs/promises'
import path from 'path'

export default class PlaylistController {
  public async dashboard({ view }: HttpContextContract) {
    const playlists = await Playlist.query().withCount('songs').orderBy('created_at', 'desc')
    return view.render('admin/dashboard', { playlists })
  }

  public async index({ view }: HttpContextContract) {
    const playlists = await Playlist.query().withCount('songs').orderBy('created_at', 'desc')
    return view.render('admin/playlists/index', { playlists })
  }

  public async create({ view }: HttpContextContract) {
    return view.render('admin/playlists/create')
  }

  public async store({ request, response, session }: HttpContextContract) {
    const { name, description } = request.only(['name', 'description'])
    
    const playlist = new Playlist()
    playlist.name = name
    playlist.description = description || null
    await playlist.save()

    // Handle file uploads
    const files = request.files('songs', {
      extnames: ['mp3'],
      size: '50mb',
    })

    let position = 0
    for (const file of files) {
      if (!file.isValid) {
        session.flash('error', `Invalid file: ${file.clientName}. Only MP3 files are allowed.`)
        continue
      }

      // Validate MP3 file
      const isValidMp3 = await this.validateMp3File(file.tmpPath!)
      if (!isValidMp3) {
        session.flash('error', `File ${file.clientName} is not a valid MP3 file.`)
        continue
      }

      const filename = `${uuidv4()}.mp3`
      await file.move(Application.publicPath('mp3'), { name: filename })

      await Song.create({
        playlistId: playlist.id,
        title: path.parse(file.clientName).name,
        filename: filename,
        originalName: file.clientName,
        position: position++,
      })
    }

    session.flash('success', 'Playlist created successfully!')
    return response.redirect('/admin/playlists')
  }

  public async edit({ params, view }: HttpContextContract) {
    const playlist = await Playlist.query()
      .where('id', params.id)
      .preload('songs', (query) => query.orderBy('position', 'asc'))
      .firstOrFail()
    
    return view.render('admin/playlists/edit', { playlist })
  }

  public async update({ params, request, response, session }: HttpContextContract) {
    const playlist = await Playlist.findOrFail(params.id)
    const { name, description, songOrder } = request.only(['name', 'description', 'songOrder'])
    
    playlist.name = name
    playlist.description = description || null
    
    // Update slug if name changed
    if (playlist.$dirty.name) {
      playlist.slug = name
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, '-')
        .replace(/(^-|-$)/g, '')
    }
    
    await playlist.save()

    // Update song positions if provided
    if (songOrder) {
      const orderArray = JSON.parse(songOrder)
      for (let i = 0; i < orderArray.length; i++) {
        await Song.query()
          .where('id', orderArray[i])
          .where('playlist_id', playlist.id)
          .update({ position: i })
      }
    }

    // Handle new file uploads
    const files = request.files('songs', {
      extnames: ['mp3'],
      size: '50mb',
    })

    if (files.length > 0) {
      const existingSongs = await Song.query()
        .where('playlist_id', playlist.id)
        .orderBy('position', 'desc')
        .first()
      
      let position = existingSongs ? existingSongs.position + 1 : 0

      for (const file of files) {
        if (!file.isValid) {
          session.flash('error', `Invalid file: ${file.clientName}. Only MP3 files are allowed.`)
          continue
        }

        const isValidMp3 = await this.validateMp3File(file.tmpPath!)
        if (!isValidMp3) {
          session.flash('error', `File ${file.clientName} is not a valid MP3 file.`)
          continue
        }

        const filename = `${uuidv4()}.mp3`
        await file.move(Application.publicPath('mp3'), { name: filename })

        await Song.create({
          playlistId: playlist.id,
          title: path.parse(file.clientName).name,
          filename: filename,
          originalName: file.clientName,
          position: position++,
        })
      }
    }

    session.flash('success', 'Playlist updated successfully!')
    return response.redirect('/admin/playlists')
  }

  public async destroy({ params, response, session }: HttpContextContract) {
    const playlist = await Playlist.query()
      .where('id', params.id)
      .preload('songs')
      .firstOrFail()
    
    // Delete associated MP3 files
    for (const song of playlist.songs) {
      try {
        await fs.unlink(Application.publicPath(`mp3/${song.filename}`))
      } catch (error) {
        console.error(`Failed to delete file: ${song.filename}`)
      }
    }
    
    await playlist.delete()
    
    session.flash('success', 'Playlist deleted successfully!')
    return response.redirect('/admin/playlists')
  }

  private async validateMp3File(filePath: string): Promise<boolean> {
    try {
      const FileType = await import('file-type')
      const buffer = await fs.readFile(filePath)
      const fileType = await FileType.fileTypeFromBuffer(buffer)
      return fileType?.mime === 'audio/mpeg'
    } catch (error) {
      return false
    }
  }
}
\`\`\`

## Song Controller

File: `app/Controllers/Http/SongController.ts`

\`\`\`typescript
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
\`\`\`

## Frontend Controller

File: `app/Controllers/Http/FrontendController.ts`

\`\`\`typescript
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
