namespace MusicManager.Models;

/// <summary>
/// Represents a playlist containing multiple songs
/// </summary>
public class Playlist
{
    public int Id { get; set; }
    
    /// <summary>
    /// Name of the playlist
    /// </summary>
    public required string Name { get; set; }
    
    /// <summary>
    /// Description of the playlist (optional)
    /// </summary>
    public string? Description { get; set; }
    
    /// <summary>
    /// When the playlist was created
    /// </summary>
    public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
    
    /// <summary>
    /// When the playlist was last updated
    /// </summary>
    public DateTime UpdatedAt { get; set; } = DateTime.UtcNow;
    
    /// <summary>
    /// Songs in this playlist (with ordering)
    /// </summary>
    public ICollection<PlaylistSong> PlaylistSongs { get; set; } = new List<PlaylistSong>();
}
