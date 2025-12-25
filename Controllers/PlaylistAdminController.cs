using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using MusicManager.Data;
using MusicManager.Models;
using MusicManager.Services;

namespace MusicManager.Controllers;

/// <summary>
/// Controller for playlist CRUD operations in admin panel
/// </summary>
public class PlaylistAdminController : Controller
{
    private readonly MusicDbContext _context;
    private readonly FileService _fileService;
    
    public PlaylistAdminController(MusicDbContext context, FileService fileService)
    {
        _context = context;
        _fileService = fileService;
    }
    
    /// <summary>
    /// List all playlists
    /// </summary>
    public async Task<IActionResult> Index()
    {
        var playlists = await _context.Playlists
            .Include(p => p.PlaylistSongs)
            .OrderByDescending(p => p.UpdatedAt)
            .ToListAsync();
        
        return View(playlists);
    }
    
    /// <summary>
    /// Display create playlist form
    /// </summary>
    [HttpGet]
    public IActionResult Create()
    {
        return View();
    }
    
    /// <summary>
    /// Process create playlist form
    /// </summary>
    [HttpPost]
    public async Task<IActionResult> Create(string name, string? description, List<IFormFile> songFiles)
    {
        if (string.IsNullOrWhiteSpace(name))
        {
            ViewBag.Error = "Playlist name is required";
            return View();
        }
        
        // Create playlist
        var playlist = new Playlist
        {
            Name = name,
            Description = description
        };
        
        _context.Playlists.Add(playlist);
        await _context.SaveChangesAsync();
        
        // Upload and add songs
        if (songFiles != null && songFiles.Count > 0)
        {
            var order = 0;
            foreach (var file in songFiles)
            {
                var validationError = _fileService.ValidateFile(file);
                if (validationError != null)
                {
                    ViewBag.Error = validationError;
                    return View();
                }
                
                var song = await _fileService.SaveFileAsync(file);
                _context.Songs.Add(song);
                await _context.SaveChangesAsync();
                
                var playlistSong = new PlaylistSong
                {
                    PlaylistId = playlist.Id,
                    SongId = song.Id,
                    Order = order++
                };
                
                _context.PlaylistSongs.Add(playlistSong);
            }
            
            await _context.SaveChangesAsync();
        }
        
        return RedirectToAction(nameof(Index));
    }
    
    /// <summary>
    /// Display edit playlist form
    /// </summary>
    [HttpGet]
    public async Task<IActionResult> Edit(int id)
    {
        var playlist = await _context.Playlists
            .Include(p => p.PlaylistSongs)
                .ThenInclude(ps => ps.Song)
            .FirstOrDefaultAsync(p => p.Id == id);
        
        if (playlist == null)
        {
            return NotFound();
        }
        
        return View(playlist);
    }
    
    /// <summary>
    /// Process edit playlist form
    /// </summary>
    [HttpPost]
    public async Task<IActionResult> Edit(int id, string name, string? description, List<IFormFile>? newSongFiles)
    {
        var playlist = await _context.Playlists
            .Include(p => p.PlaylistSongs)
            .FirstOrDefaultAsync(p => p.Id == id);
        
        if (playlist == null)
        {
            return NotFound();
        }
        
        playlist.Name = name;
        playlist.Description = description;
        playlist.UpdatedAt = DateTime.UtcNow;
        
        // Add new songs if provided
        if (newSongFiles != null && newSongFiles.Count > 0)
        {
            var maxOrder = playlist.PlaylistSongs.Any() 
                ? playlist.PlaylistSongs.Max(ps => ps.Order) 
                : -1;
            
            foreach (var file in newSongFiles)
            {
                var validationError = _fileService.ValidateFile(file);
                if (validationError != null)
                {
                    ViewBag.Error = validationError;
                    return await Edit(id);
                }
                
                var song = await _fileService.SaveFileAsync(file);
                _context.Songs.Add(song);
                await _context.SaveChangesAsync();
                
                var playlistSong = new PlaylistSong
                {
                    PlaylistId = playlist.Id,
                    SongId = song.Id,
                    Order = ++maxOrder
                };
                
                _context.PlaylistSongs.Add(playlistSong);
            }
        }
        
        await _context.SaveChangesAsync();
        
        return RedirectToAction(nameof(Index));
    }
    
    /// <summary>
    /// Update song order in playlist (called via AJAX)
    /// </summary>
    [HttpPost]
    public async Task<IActionResult> UpdateOrder(int playlistId, [FromBody] List<int> songIds)
    {
        var playlist = await _context.Playlists
            .Include(p => p.PlaylistSongs)
            .FirstOrDefaultAsync(p => p.Id == playlistId);
        
        if (playlist == null)
        {
            return NotFound();
        }
        
        for (int i = 0; i < songIds.Count; i++)
        {
            var playlistSong = playlist.PlaylistSongs.FirstOrDefault(ps => ps.SongId == songIds[i]);
            if (playlistSong != null)
            {
                playlistSong.Order = i;
            }
        }
        
        playlist.UpdatedAt = DateTime.UtcNow;
        await _context.SaveChangesAsync();
        
        return Ok();
    }
    
    /// <summary>
    /// Remove song from playlist
    /// </summary>
    [HttpPost]
    public async Task<IActionResult> RemoveSong(int playlistId, int songId)
    {
        var playlistSong = await _context.PlaylistSongs
            .FirstOrDefaultAsync(ps => ps.PlaylistId == playlistId && ps.SongId == songId);
        
        if (playlistSong != null)
        {
            _context.PlaylistSongs.Remove(playlistSong);
            
            // Update playlist timestamp
            var playlist = await _context.Playlists.FindAsync(playlistId);
            if (playlist != null)
            {
                playlist.UpdatedAt = DateTime.UtcNow;
            }
            
            await _context.SaveChangesAsync();
            
            // Reorder remaining songs
            var remainingSongs = await _context.PlaylistSongs
                .Where(ps => ps.PlaylistId == playlistId)
                .OrderBy(ps => ps.Order)
                .ToListAsync();
            
            for (int i = 0; i < remainingSongs.Count; i++)
            {
                remainingSongs[i].Order = i;
            }
            
            await _context.SaveChangesAsync();
        }
        
        return RedirectToAction(nameof(Edit), new { id = playlistId });
    }
    
    /// <summary>
    /// Display delete confirmation
    /// </summary>
    [HttpGet]
    public async Task<IActionResult> Delete(int id)
    {
        var playlist = await _context.Playlists
            .Include(p => p.PlaylistSongs)
                .ThenInclude(ps => ps.Song)
            .FirstOrDefaultAsync(p => p.Id == id);
        
        if (playlist == null)
        {
            return NotFound();
        }
        
        return View(playlist);
    }
    
    /// <summary>
    /// Process delete playlist
    /// </summary>
    [HttpPost, ActionName("Delete")]
    public async Task<IActionResult> DeleteConfirmed(int id)
    {
        var playlist = await _context.Playlists
            .Include(p => p.PlaylistSongs)
                .ThenInclude(ps => ps.Song)
            .FirstOrDefaultAsync(p => p.Id == id);
        
        if (playlist == null)
        {
            return NotFound();
        }
        
        // Note: We don't delete the actual Song records or files as they might be used in other playlists
        // Only the PlaylistSong associations are deleted via cascade
        
        _context.Playlists.Remove(playlist);
        await _context.SaveChangesAsync();
        
        return RedirectToAction(nameof(Index));
    }
}
