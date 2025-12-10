# Models

## Playlist Model

File: `app/Models/Playlist.ts`

\`\`\`typescript
import { DateTime } from 'luxon'
import { BaseModel, column, hasMany, HasMany, beforeCreate } from '@ioc:Adonis/Lucid/Orm'
import Song from './Song'

export default class Playlist extends BaseModel {
  @column({ isPrimary: true })
  public id: number

  @column()
  public name: string

  @column()
  public slug: string

  @column()
  public description: string | null

  @column.dateTime({ autoCreate: true })
  public createdAt: DateTime

  @column.dateTime({ autoCreate: true, autoUpdate: true })
  public updatedAt: DateTime

  @hasMany(() => Song)
  public songs: HasMany<typeof Song>

  @beforeCreate()
  public static async generateSlug(playlist: Playlist) {
    playlist.slug = playlist.name
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/(^-|-$)/g, '')
    
    // Ensure unique slug
    const existing = await Playlist.query().where('slug', playlist.slug).first()
    if (existing) {
      playlist.slug = `${playlist.slug}-${Date.now()}`
    }
  }
}
\`\`\`

## Song Model

File: `app/Models/Song.ts`

\`\`\`typescript
import { DateTime } from 'luxon'
import { BaseModel, column, belongsTo, BelongsTo } from '@ioc:Adonis/Lucid/Orm'
import Playlist from './Playlist'

export default class Song extends BaseModel {
  @column({ isPrimary: true })
  public id: number

  @column()
  public playlistId: number

  @column()
  public title: string

  @column()
  public filename: string

  @column()
  public originalName: string

  @column()
  public position: number

  @column.dateTime({ autoCreate: true })
  public createdAt: DateTime

  @column.dateTime({ autoCreate: true, autoUpdate: true })
  public updatedAt: DateTime

  @belongsTo(() => Playlist)
  public playlist: BelongsTo<typeof Playlist>
}
