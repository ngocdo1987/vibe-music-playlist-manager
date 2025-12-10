# Database Migrations

## Migration: Create Playlists Table

File: `database/migrations/xxxx_create_playlists_table.ts`

\`\`\`typescript
import BaseSchema from '@ioc:Adonis/Lucid/Schema'

export default class extends BaseSchema {
  protected tableName = 'playlists'

  public async up() {
    this.schema.createTable(this.tableName, (table) => {
      table.increments('id').primary()
      table.string('name', 255).notNullable()
      table.string('slug', 255).notNullable().unique()
      table.text('description').nullable()
      table.timestamp('created_at', { useTz: true })
      table.timestamp('updated_at', { useTz: true })
    })
  }

  public async down() {
    this.schema.dropTable(this.tableName)
  }
}
\`\`\`

## Migration: Create Songs Table

File: `database/migrations/xxxx_create_songs_table.ts`

\`\`\`typescript
import BaseSchema from '@ioc:Adonis/Lucid/Schema'

export default class extends BaseSchema {
  protected tableName = 'songs'

  public async up() {
    this.schema.createTable(this.tableName, (table) => {
      table.increments('id').primary()
      table.integer('playlist_id').unsigned().references('id').inTable('playlists').onDelete('CASCADE')
      table.string('title', 255).notNullable()
      table.string('filename', 255).notNullable()
      table.string('original_name', 255).notNullable()
      table.integer('position').notNullable().defaultTo(0)
      table.timestamp('created_at', { useTz: true })
      table.timestamp('updated_at', { useTz: true })
    })
  }

  public async down() {
    this.schema.dropTable(this.tableName)
  }
}
