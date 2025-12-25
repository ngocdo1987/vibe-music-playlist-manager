using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using MusicManager.Data;

namespace MusicManager.Controllers;

/// <summary>
/// Controller for public-facing pages (no authentication required)
/// </summary>
public class PublicController : Controller
{
    private readonly MusicDbContext _context;
    
    public PublicController(MusicDbContext context)
    {
        _context = context;
    }
    
    /// <summary>
    /// Display list of all playlists
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
    /// Display playlist player
    /// </summary>
    public async Task<IActionResult> Player(int id)
    {
        var playlist = await _context.Playlists
            .Include(p => p.PlaylistSongs)
                .ThenInclude(ps => ps.Song)
            .FirstOrDefaultAsync(p => p.Id == id);
        
        if (playlist == null)
        {
            return NotFound();
        }
        
        // Sort songs by order
        playlist.PlaylistSongs = playlist.PlaylistSongs.OrderBy(ps => ps.Order).ToList();
        
        return View(playlist);
    }
}
