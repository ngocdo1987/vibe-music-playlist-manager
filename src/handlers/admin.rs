use actix_session::Session;
use actix_web::{web, HttpResponse, Result};
use tera::Context;

use crate::db;
use crate::handlers::auth::is_logged_in;
use crate::models::PlaylistForm;
use crate::AppState;

fn require_auth(session: &Session) -> Option<HttpResponse> {
    if !is_logged_in(session) {
        Some(
            HttpResponse::Found()
                .append_header(("Location", "/admin/login"))
                .finish(),
        )
    } else {
        None
    }
}

fn slugify(text: &str) -> String {
    text.to_lowercase()
        .chars()
        .map(|c| {
            if c.is_alphanumeric() {
                c
            } else {
                '-'
            }
        })
        .collect::<String>()
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

pub async fn dashboard(data: web::Data<AppState>, session: Session) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let conn = data.db.lock().unwrap();
    let playlists = db::get_all_playlists(&conn)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;
    let songs = db::get_all_songs(&conn)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    let mut ctx = Context::new();
    ctx.insert("playlists", &playlists);
    ctx.insert("songs", &songs);
    ctx.insert("playlist_count", &playlists.len());
    ctx.insert("song_count", &songs.len());

    let rendered = data
        .tera
        .render("admin/dashboard.html", &ctx)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
}

pub async fn playlists(data: web::Data<AppState>, session: Session) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let conn = data.db.lock().unwrap();
    let playlists = db::get_all_playlists(&conn)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    let mut ctx = Context::new();
    ctx.insert("playlists", &playlists);

    let rendered = data
        .tera
        .render("admin/playlists.html", &ctx)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
}

pub async fn playlist_new(data: web::Data<AppState>, session: Session) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let conn = data.db.lock().unwrap();
    let songs = db::get_all_songs(&conn)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    let mut ctx = Context::new();
    ctx.insert("songs", &songs);
    ctx.insert("playlist", &serde_json::json!(null));
    ctx.insert("playlist_songs", &Vec::<i64>::new());

    let rendered = data
        .tera
        .render("admin/playlist_form.html", &ctx)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
}

pub async fn playlist_create(
    data: web::Data<AppState>,
    form: web::Form<PlaylistForm>,
    session: Session,
) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let slug = slugify(&form.name);
    let conn = data.db.lock().unwrap();

    db::create_playlist(&conn, &form.name, &slug, form.description.as_deref())
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Found()
        .append_header(("Location", "/admin/playlists"))
        .finish())
}

pub async fn playlist_edit(
    data: web::Data<AppState>,
    path: web::Path<i64>,
    session: Session,
) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let playlist_id = path.into_inner();
    let conn = data.db.lock().unwrap();

    let playlist = db::get_playlist_by_id(&conn, playlist_id)
        .map_err(|_| actix_web::error::ErrorNotFound("Playlist not found"))?;

    let songs = db::get_all_songs(&conn)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    let playlist_songs = db::get_playlist_songs(&conn, playlist_id)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    let playlist_song_ids: Vec<i64> = playlist_songs.iter().map(|s| s.id).collect();

    let mut ctx = Context::new();
    ctx.insert("playlist", &playlist);
    ctx.insert("songs", &songs);
    ctx.insert("playlist_songs", &playlist_songs);
    ctx.insert("playlist_song_ids", &playlist_song_ids);

    let rendered = data
        .tera
        .render("admin/playlist_form.html", &ctx)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
}

pub async fn playlist_update(
    data: web::Data<AppState>,
    path: web::Path<i64>,
    form: web::Form<PlaylistForm>,
    session: Session,
) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let playlist_id = path.into_inner();
    let slug = slugify(&form.name);
    let conn = data.db.lock().unwrap();

    db::update_playlist(&conn, playlist_id, &form.name, &slug, form.description.as_deref())
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Found()
        .append_header(("Location", "/admin/playlists"))
        .finish())
}

pub async fn playlist_delete(
    data: web::Data<AppState>,
    path: web::Path<i64>,
    session: Session,
) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let playlist_id = path.into_inner();
    let conn = data.db.lock().unwrap();

    db::delete_playlist(&conn, playlist_id)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Found()
        .append_header(("Location", "/admin/playlists"))
        .finish())
}

pub async fn songs(data: web::Data<AppState>, session: Session) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let conn = data.db.lock().unwrap();
    let songs = db::get_all_songs(&conn)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    let mut ctx = Context::new();
    ctx.insert("songs", &songs);

    let rendered = data
        .tera
        .render("admin/songs.html", &ctx)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
}

pub async fn song_delete(
    data: web::Data<AppState>,
    path: web::Path<i64>,
    session: Session,
) -> Result<HttpResponse> {
    if let Some(redirect) = require_auth(&session) {
        return Ok(redirect);
    }

    let song_id = path.into_inner();
    let conn = data.db.lock().unwrap();

    let filename = db::delete_song(&conn, song_id)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    // Delete file from disk
    let file_path = format!("mp3/{}", filename);
    let _ = std::fs::remove_file(file_path);

    Ok(HttpResponse::Found()
        .append_header(("Location", "/admin/songs"))
        .finish())
}
