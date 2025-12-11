use actix_web::{web, HttpResponse, Result};
use tera::Context;

use crate::db;
use crate::AppState;

pub async fn index(data: web::Data<AppState>) -> Result<HttpResponse> {
    let conn = data.db.lock().unwrap();
    let playlists = db::get_all_playlists(&conn)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;
    
    let mut ctx = Context::new();
    ctx.insert("playlists", &playlists);
    
    let rendered = data.tera.render("public/index.html", &ctx)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;
    
    Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
}

pub async fn playlist(
    data: web::Data<AppState>,
    path: web::Path<String>,
) -> Result<HttpResponse> {
    let slug = path.into_inner();
    let conn = data.db.lock().unwrap();
    
    let playlist = db::get_playlist_by_slug(&conn, &slug)
        .map_err(|_| actix_web::error::ErrorNotFound("Playlist not found"))?;
    
    let songs = db::get_playlist_songs(&conn, playlist.id)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;
    
    let mut ctx = Context::new();
    ctx.insert("playlist", &playlist);
    ctx.insert("songs", &songs);
    
    let rendered = data.tera.render("public/playlist.html", &ctx)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;
    
    Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
}
