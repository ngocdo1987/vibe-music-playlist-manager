namespace MusicManager.Services;

/// <summary>
/// Service for handling authentication with credentials from .env file
/// </summary>
public class AuthService
{
    private readonly string _adminUsername;
    private readonly string _adminPassword;
    
    public AuthService()
    {
        // Load credentials from environment variables
        _adminUsername = Environment.GetEnvironmentVariable("ADMIN_USERNAME") ?? "admin";
        _adminPassword = Environment.GetEnvironmentVariable("ADMIN_PASSWORD") ?? "admin";
    }
    
    /// <summary>
    /// Validates admin credentials
    /// </summary>
    /// <param name="username">Username to validate</param>
    /// <param name="password">Password to validate</param>
    /// <returns>True if credentials are valid</returns>
    public bool ValidateCredentials(string username, string password)
    {
        return username == _adminUsername && password == _adminPassword;
    }
}
