// Multipart form data parser for file uploads
const std = @import("std");

/// Parsed file from multipart form
pub const UploadedFile = struct {
    name: []const u8, // form field name
    filename: []const u8, // original filename
    content_type: []const u8,
    data: []const u8, // file content
};

/// Parse result containing fields and files
pub const MultipartData = struct {
    fields: std.StringHashMap([]const u8),
    files: []UploadedFile,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *MultipartData) void {
        self.fields.deinit();
        self.allocator.free(self.files);
    }
};

/// Extract boundary from Content-Type header
pub fn getBoundary(content_type: []const u8) ?[]const u8 {
    // Content-Type: multipart/form-data; boundary=----WebKitFormBoundary...
    if (std.mem.indexOf(u8, content_type, "boundary=")) |start| {
        const boundary_start = start + "boundary=".len;
        // Find end (semicolon, space, or end of string)
        var end = content_type.len;
        for (content_type[boundary_start..], 0..) |ch, i| {
            if (ch == ';' or ch == ' ' or ch == '\r' or ch == '\n') {
                end = boundary_start + i;
                break;
            }
        }
        return content_type[boundary_start..end];
    }
    return null;
}

/// Parse multipart form data
pub fn parse(allocator: std.mem.Allocator, content_type: []const u8, body: []const u8) !MultipartData {
    var fields = std.StringHashMap([]const u8).init(allocator);
    errdefer fields.deinit();

    // Get boundary
    const boundary = getBoundary(content_type) orelse return MultipartData{
        .fields = fields,
        .files = &[_]UploadedFile{},
        .allocator = allocator,
    };

    // Boundary markers
    var full_boundary: [256]u8 = undefined;
    const prefix = "--";
    @memcpy(full_boundary[0..prefix.len], prefix);
    @memcpy(full_boundary[prefix.len..][0..boundary.len], boundary);
    const full_boundary_slice = full_boundary[0 .. prefix.len + boundary.len];

    // Collect files
    var files_list: [16]UploadedFile = undefined;
    var file_count: usize = 0;

    // Split by boundary
    var parts = std.mem.splitSequence(u8, body, full_boundary_slice);

    while (parts.next()) |part| {
        // Skip empty parts and final boundary
        if (part.len < 10) continue;
        if (std.mem.startsWith(u8, part, "--")) continue;

        // Find headers/content separator
        const header_end = std.mem.indexOf(u8, part, "\r\n\r\n") orelse continue;
        const headers = part[0..header_end];
        const content_start = header_end + 4;
        var content = part[content_start..];

        // Trim trailing CRLF
        while (content.len > 0 and (content[content.len - 1] == '\r' or content[content.len - 1] == '\n')) {
            content = content[0 .. content.len - 1];
        }

        // Parse Content-Disposition header
        var field_name: []const u8 = "";
        var filename: ?[]const u8 = null;
        var file_content_type: []const u8 = "application/octet-stream";

        var header_lines = std.mem.splitSequence(u8, headers, "\r\n");
        while (header_lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r\n");
            if (trimmed.len == 0) continue;

            if (std.mem.startsWith(u8, trimmed, "Content-Disposition:")) {
                // Parse name and filename
                if (std.mem.indexOf(u8, trimmed, "name=\"")) |name_start| {
                    const start = name_start + "name=\"".len;
                    if (std.mem.indexOf(u8, trimmed[start..], "\"")) |end| {
                        field_name = trimmed[start .. start + end];
                    }
                }
                if (std.mem.indexOf(u8, trimmed, "filename=\"")) |fn_start| {
                    const start = fn_start + "filename=\"".len;
                    if (std.mem.indexOf(u8, trimmed[start..], "\"")) |end| {
                        filename = trimmed[start .. start + end];
                    }
                }
            } else if (std.mem.startsWith(u8, trimmed, "Content-Type:")) {
                file_content_type = std.mem.trim(u8, trimmed["Content-Type:".len..], " \t");
            }
        }

        // Store field or file
        if (filename) |fname| {
            // It's a file
            if (file_count < files_list.len) {
                files_list[file_count] = .{
                    .name = field_name,
                    .filename = fname,
                    .content_type = file_content_type,
                    .data = content,
                };
                file_count += 1;
            }
        } else {
            // It's a regular field
            try fields.put(field_name, content);
        }
    }

    // Copy files to allocated slice
    const files = try allocator.alloc(UploadedFile, file_count);
    @memcpy(files, files_list[0..file_count]);

    return MultipartData{
        .fields = fields,
        .files = files,
        .allocator = allocator,
    };
}

test "getBoundary" {
    const ct = "multipart/form-data; boundary=----WebKitFormBoundary123";
    const boundary = getBoundary(ct);
    try std.testing.expect(boundary != null);
    try std.testing.expectEqualStrings("----WebKitFormBoundary123", boundary.?);
}
