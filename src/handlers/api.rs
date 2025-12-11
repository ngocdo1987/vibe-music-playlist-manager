use actix_multipart::Multipart;
use actix_session::Session;
use actix_web::{web, HttpResponse, Result};
use futures_util::StreamExt;
use serde_json::json;
use std::io::Write;
use uuid::Uuid;

use crate::db;
use crate::handlers::auth::is_logged_in;
use crate::models::UpdatePlaylistSongsRequest;
use crate::AppState;

// MP3 file magic bytes
const MP3_MAGIC_ID3: [u8; 3] = [0x49, 0x44, 0x33]; // ID3
const MP3_MAGIC_SYNC: [u8; 2] = [0xFF, 0xFB]; // Frame sync

fn is_valid_mp3(data: &[u8]) -> bool {
    if data.len() < 3 {
        return false;
    }
    
    // Check for ID3 tag
    if data[0..3] == MP3_MAGIC_ID3 {
        return true;
    }
    
    // Check for frame sync
    if data.len() >= 2 && data[0] == MP3_MAGIC_SYNC[0] && (data[1] & 0xE0) == 0xE0 {
        return true;
    }
    
    false
}

pub async fn upload_song(
    data: web::Data<AppState>,
    mut payload: Multipart,
    session: Session,
) -> Result<HttpResponse> {
    if !is_logged_in(&session) {
        return Ok(HttpResponse::Unauthorized().json(json!({"error": "Unauthorized"})));
    }

    let mut uploaded_songs = Vec::new();

    while let Some(item) = payload.next().await {
        let mut field = item.map_err(|e| actix_web::error::ErrorBadRequest(e))?;
        
        let content_disposition = field.content_disposition();
        let original_filename = content_disposition
            .get_filename()
            .map(|f| f.to_string())
            .unwrap_or_else(|| "unknown.mp3".to_string());

        // Collect file data
        let mut file_data = Vec::new();
        while let Some(chunk) = field.next().await {
            let chunk = chunk.map_err(|e| actix_web::error::ErrorBadRequest(e))?;
            file_data.extend_from_slice(&chunk);
        }

        // Validate MP3 format
        if !is_valid_mp3(&file_data) {
            return Ok(HttpResponse::BadRequest().json(json!({
                "error": format!("File '{}' is not a valid MP3 file", original_filename)
            })));
        }

        // Generate unique filename
        let extension = "mp3";
        let unique_filename = format!("{}.{}", Uuid::new_v4(), extension);
        let file_path = format!("mp3/{}", unique_filename);

        // Save file
        let mut file = std::fs::File::create(&file_path)
            .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;
        file.write_all(&file_data)
            .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

        // Save to database
        let conn = data.db.lock().unwrap();
        let song_id = db::create_song(&conn, &unique_filename, &original_filename)
            .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

        uploaded_songs.push(json!({
            "id": song_id,
            "filename": unique_filename,
            "original_name": original_filename
        }));
    }

    Ok(HttpResponse::Ok().json(json!({
        "success": true,
        "songs": uploaded_songs
    })))
}

pub async fn update_playlist_songs(
    data: web::Data<AppState>,
    path: web::Path<i64>,
    body: web::Json<UpdatePlaylistSongsRequest>,
    session: Session,
) -> Result<HttpResponse> {
    if !is_logged_in(&session) {
        return Ok(HttpResponse::Unauthorized().json(json!({"error": "Unauthorized"})));
    }

    let playlist_id = path.into_inner();
    let conn = data.db.lock().unwrap();

    db::update_playlist_songs(&conn, playlist_id, &body.song_ids)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;

    Ok(HttpResponse::Ok().json(json!({"success": true})))
}
