use actix_files as fs;
use actix_session::{config::PersistentSession, storage::CookieSessionStore, SessionMiddleware};
use actix_web::{cookie::Key, middleware::Logger, web, App, HttpServer};
use dotenv::dotenv;
use std::env;
use std::sync::Mutex;
use tera::Tera;

mod db;
mod handlers;
mod models;

pub struct AppState {
    pub db: Mutex<rusqlite::Connection>,
    pub tera: Tera,
    pub admin_username: String,
    pub admin_password: String,
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    dotenv().ok();
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    // Create mp3 directory if not exists
    std::fs::create_dir_all("mp3").expect("Failed to create mp3 directory");

    // Initialize database
    let conn = db::init_db().expect("Failed to initialize database");

    // Load templates
    let tera = Tera::new("templates/**/*").expect("Failed to load templates");

    // Get configuration from environment
    let admin_username = env::var("ADMIN_USERNAME").unwrap_or_else(|_| "admin".to_string());
    let admin_password = env::var("ADMIN_PASSWORD").unwrap_or_else(|_| "password".to_string());
    let host = env::var("HOST").unwrap_or_else(|_| "127.0.0.1".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "8080".to_string());
    let session_secret = env::var("SESSION_SECRET")
        .unwrap_or_else(|_| "a".repeat(64));

    let app_state = web::Data::new(AppState {
        db: Mutex::new(conn),
        tera,
        admin_username,
        admin_password,
    });

    let secret_key = Key::from(session_secret.as_bytes());

    println!("Server starting at http://{}:{}", host, port);

    HttpServer::new(move || {
        App::new()
            .app_data(app_state.clone())
            .wrap(Logger::default())
            .wrap(
                SessionMiddleware::builder(CookieSessionStore::default(), secret_key.clone())
                    .session_lifecycle(PersistentSession::default())
                    .cookie_secure(false)
                    .build(),
            )
            // Static files
            .service(fs::Files::new("/static", "static").show_files_listing())
            .service(fs::Files::new("/mp3", "mp3").show_files_listing())
            // Public routes
            .route("/", web::get().to(handlers::public::index))
            .route("/playlist/{slug}", web::get().to(handlers::public::playlist))
            // Auth routes
            .route("/admin/login", web::get().to(handlers::auth::login_page))
            .route("/admin/login", web::post().to(handlers::auth::login))
            .route("/admin/logout", web::get().to(handlers::auth::logout))
            // Admin routes
            .route("/admin", web::get().to(handlers::admin::dashboard))
            .route("/admin/playlists", web::get().to(handlers::admin::playlists))
            .route("/admin/playlists/new", web::get().to(handlers::admin::playlist_new))
            .route("/admin/playlists/new", web::post().to(handlers::admin::playlist_create))
            .route("/admin/playlists/{id}/edit", web::get().to(handlers::admin::playlist_edit))
            .route("/admin/playlists/{id}/edit", web::post().to(handlers::admin::playlist_update))
            .route("/admin/playlists/{id}/delete", web::post().to(handlers::admin::playlist_delete))
            .route("/admin/songs", web::get().to(handlers::admin::songs))
            .route("/admin/songs/{id}/delete", web::post().to(handlers::admin::song_delete))
            // API routes
            .route("/api/upload", web::post().to(handlers::api::upload_song))
            .route("/api/playlists/{id}/songs", web::post().to(handlers::api::update_playlist_songs))
    })
    .bind(format!("{}:{}", host, port))?
    .run()
    .await
}
