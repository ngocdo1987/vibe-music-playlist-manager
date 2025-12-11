use actix_session::Session;
use actix_web::{web, HttpResponse, Result};
use tera::Context;

use crate::models::LoginForm;
use crate::AppState;

pub async fn login_page(data: web::Data<AppState>) -> Result<HttpResponse> {
    let ctx = Context::new();
    let rendered = data.tera.render("auth/login.html", &ctx)
        .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;
    
    Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
}

pub async fn login(
    data: web::Data<AppState>,
    form: web::Form<LoginForm>,
    session: Session,
) -> Result<HttpResponse> {
    if form.username == data.admin_username && form.password == data.admin_password {
        session.insert("logged_in", true)?;
        Ok(HttpResponse::Found()
            .append_header(("Location", "/admin"))
            .finish())
    } else {
        let mut ctx = Context::new();
        ctx.insert("error", "Invalid username or password");
        let rendered = data.tera.render("auth/login.html", &ctx)
            .map_err(|e| actix_web::error::ErrorInternalServerError(e))?;
        
        Ok(HttpResponse::Ok().content_type("text/html").body(rendered))
    }
}

pub async fn logout(session: Session) -> Result<HttpResponse> {
    session.purge();
    Ok(HttpResponse::Found()
        .append_header(("Location", "/admin/login"))
        .finish())
}

pub fn is_logged_in(session: &Session) -> bool {
    session.get::<bool>("logged_in").unwrap_or(None).unwrap_or(false)
}
