namespace MusicManager.Middleware;

/// <summary>
/// Middleware to protect admin routes with authentication
/// </summary>
public class AdminAuthMiddleware
{
    private readonly RequestDelegate _next;
    
    public AdminAuthMiddleware(RequestDelegate next)
    {
        _next = next;
    }
    
    public async Task InvokeAsync(HttpContext context)
    {
        var path = context.Request.Path.Value?.ToLower() ?? "";
        
        // Check if this is an admin route (but not the login page)
        if (path.StartsWith("/admin") || path.StartsWith("/playlistadmin"))
        {
            // Check if user is authenticated
            var isAuthenticated = context.Session.GetString("IsAuthenticated") == "true";
            
            if (!isAuthenticated)
            {
                // Redirect to login page
                context.Response.Redirect("/auth/login");
                return;
            }
        }
        
        await _next(context);
    }
}
