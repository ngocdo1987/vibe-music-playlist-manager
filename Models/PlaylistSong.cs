namespace MusicManager.Models;

/// <summary>
/// Junction table for many-to-many relationship between Playlist and Song
/// Also stores the order of songs within a playlist
/// </summary>
public class PlaylistSong
{
    public int Id { get; set; }
    
    public int PlaylistId { get; set; }
    public Playlist Playlist { get; set; } = null!;
    
    public int SongId { get; set; }
    public Song Song { get; set; } = null!;
    
    /// <summary>
    /// Order of this song within the playlist (0-indexed)
    /// </summary>
    public int Order { get; set; }
}
