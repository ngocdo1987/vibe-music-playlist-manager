# Routes

File: `start/routes.ts`

\`\`\`typescript
import Route from '@ioc:Adonis/Core/Route'

// Frontend Routes (No authentication required)
Route.get('/', 'FrontendController.home').as('home')
Route.get('/playlist/:slug', 'FrontendController.playlist').as('playlist.show')

// Admin Auth Routes
Route.group(() => {
  Route.get('/login', 'AuthController.showLogin').as('admin.login')
  Route.post('/login', 'AuthController.login').as('admin.login.post')
  Route.post('/logout', 'AuthController.logout').as('admin.logout')
}).prefix('/admin')

// Admin Protected Routes
Route.group(() => {
  Route.get('/dashboard', 'PlaylistController.dashboard').as('admin.dashboard')
  
  // Playlist CRUD
  Route.get('/playlists', 'PlaylistController.index').as('admin.playlists.index')
  Route.get('/playlists/create', 'PlaylistController.create').as('admin.playlists.create')
  Route.post('/playlists', 'PlaylistController.store').as('admin.playlists.store')
  Route.get('/playlists/:id/edit', 'PlaylistController.edit').as('admin.playlists.edit')
  Route.put('/playlists/:id', 'PlaylistController.update').as('admin.playlists.update')
  Route.delete('/playlists/:id', 'PlaylistController.destroy').as('admin.playlists.destroy')
  
  // Song management
  Route.delete('/songs/:id', 'SongController.destroy').as('admin.songs.destroy')
  Route.put('/songs/:id/title', 'SongController.updateTitle').as('admin.songs.updateTitle')
})
  .prefix('/admin')
  .middleware('adminAuth')
