#include "filters/AuthFilter.h"

using namespace filters;
using namespace drogon;

void AuthFilter::doFilter(const HttpRequestPtr& req,
                          FilterCallback&& fcb,
                          FilterChainCallback&& fccb) {
    auto session = req->session();
    
    if (session && session->find("authenticated")) {
        // User is authenticated, continue
        fccb();
        return;
    }
    
    // Not authenticated, redirect to login
    auto resp = HttpResponse::newRedirectionResponse("/admin/login");
    fcb(resp);
}