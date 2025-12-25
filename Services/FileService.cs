using MusicManager.Models;

namespace MusicManager.Services;

/// <summary>
/// Service for handling file uploads and validation
/// </summary>
public class FileService
{
    private readonly string _uploadPath;
    private readonly long _maxFileSizeBytes;
    private readonly string[] _allowedExtensions = { ".mp3" };
    private readonly string[] _allowedMimeTypes = { "audio/mpeg", "audio/mp3" };
    
    public FileService(IConfiguration configuration)
    {
        _uploadPath = configuration["FileUpload:UploadPath"] ?? "wwwroot/mp3";
        var maxFileSizeMB = configuration.GetValue<int>("FileUpload:MaxFileSizeMB", 50);
        _maxFileSizeBytes = maxFileSizeMB * 1024 * 1024;
        
        // Ensure upload directory exists
        if (!Directory.Exists(_uploadPath))
        {
            Directory.CreateDirectory(_uploadPath);
        }
    }
    
    /// <summary>
    /// Validates if the uploaded file is a valid MP3 file
    /// </summary>
    /// <param name="file">The uploaded file</param>
    /// <returns>Error message if validation fails, null if valid</returns>
    public string? ValidateFile(IFormFile file)
    {
        if (file == null || file.Length == 0)
        {
            return "No file uploaded";
        }
        
        if (file.Length > _maxFileSizeBytes)
        {
            return $"File size exceeds maximum allowed size of {_maxFileSizeBytes / (1024 * 1024)}MB";
        }
        
        var extension = Path.GetExtension(file.FileName).ToLowerInvariant();
        if (!_allowedExtensions.Contains(extension))
        {
            return $"Invalid file type. Only {string.Join(", ", _allowedExtensions)} files are allowed";
        }
        
        if (!_allowedMimeTypes.Contains(file.ContentType.ToLowerInvariant()))
        {
            return $"Invalid file content type. Expected audio/mpeg, got {file.ContentType}";
        }
        
        return null;
    }
    
    /// <summary>
    /// Saves an uploaded file and returns the Song entity
    /// </summary>
    /// <param name="file">The uploaded file</param>
    /// <returns>Song entity with file information</returns>
    public async Task<Song> SaveFileAsync(IFormFile file)
    {
        // Generate unique filename to prevent conflicts
        var uniqueFileName = $"{Guid.NewGuid()}{Path.GetExtension(file.FileName)}";
        var filePath = Path.Combine(_uploadPath, uniqueFileName);
        
        // Save file to disk
        using (var stream = new FileStream(filePath, FileMode.Create))
        {
            await file.CopyToAsync(stream);
        }
        
        // Extract title from filename (remove extension)
        var title = Path.GetFileNameWithoutExtension(file.FileName);
        
        // Create Song entity
        var song = new Song
        {
            FileName = file.FileName,
            FilePath = $"/mp3/{uniqueFileName}",
            Title = title
        };
        
        return song;
    }
    
    /// <summary>
    /// Deletes a song file from disk
    /// </summary>
    /// <param name="filePath">Relative path to the file (e.g., /mp3/file.mp3)</param>
    public void DeleteFile(string filePath)
    {
        try
        {
            var fullPath = Path.Combine("wwwroot", filePath.TrimStart('/'));
            if (File.Exists(fullPath))
            {
                File.Delete(fullPath);
            }
        }
        catch (Exception)
        {
            // Ignore errors during file deletion
        }
    }
}
