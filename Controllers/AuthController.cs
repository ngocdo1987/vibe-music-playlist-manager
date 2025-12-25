using Microsoft.AspNetCore.Mvc;
using MusicManager.Services;

namespace MusicManager.Controllers;

/// <summary>
/// Handles authentication (login/logout) for admin users
/// </summary>
public class AuthController : Controller
{
    private readonly AuthService _authService;
    
    public AuthController(AuthService authService)
    {
        _authService = authService;
    }
    
    /// <summary>
    /// Display login page
    /// </summary>
    [HttpGet]
    public IActionResult Login()
    {
        // If already authenticated, redirect to admin dashboard
        if (HttpContext.Session.GetString("IsAuthenticated") == "true")
        {
            return RedirectToAction("Index", "Admin");
        }
        
        return View();
    }
    
    /// <summary>
    /// Process login form submission
    /// </summary>
    [HttpPost]
    public IActionResult Login(string username, string password)
    {
        if (_authService.ValidateCredentials(username, password))
        {
            // Set session variable to indicate authenticated user
            HttpContext.Session.SetString("IsAuthenticated", "true");
            HttpContext.Session.SetString("Username", username);
            
            return RedirectToAction("Index", "Admin");
        }
        
        ViewBag.Error = "Invalid username or password";
        return View();
    }
    
    /// <summary>
    /// Logout and clear session
    /// </summary>
    [HttpPost]
    public IActionResult Logout()
    {
        HttpContext.Session.Clear();
        return RedirectToAction("Index", "Public");
    }
}
