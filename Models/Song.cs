namespace MusicManager.Models;

/// <summary>
/// Represents a song (MP3 file) in the system
/// </summary>
public class Song
{
    public int Id { get; set; }
    
    /// <summary>
    /// Original filename when uploaded
    /// </summary>
    public required string FileName { get; set; }
    
    /// <summary>
    /// Path to the file on the server (relative to wwwroot)
    /// </summary>
    public required string FilePath { get; set; }
    
    /// <summary>
    /// Display title of the song
    /// </summary>
    public required string Title { get; set; }
    
    /// <summary>
    /// Duration of the song in seconds (optional)
    /// </summary>
    public int? DurationSeconds { get; set; }
    
    /// <summary>
    /// When the song was uploaded
    /// </summary>
    public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
    
    /// <summary>
    /// Playlists that contain this song
    /// </summary>
    public ICollection<PlaylistSong> PlaylistSongs { get; set; } = new List<PlaylistSong>();
}
