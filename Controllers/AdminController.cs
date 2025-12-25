using Microsoft.AspNetCore.Mvc;

namespace MusicManager.Controllers;

/// <summary>
/// Admin dashboard controller
/// </summary>
public class AdminController : Controller
{
    /// <summary>
    /// Display admin dashboard
    /// </summary>
    public IActionResult Index()
    {
        ViewBag.Username = HttpContext.Session.GetString("Username");
        return View();
    }
}
