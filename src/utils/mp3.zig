// MP3 validation utilities
const std = @import("std");

/// Check if data is a valid MP3 file
/// Checks for ID3v2 header or MP3 frame sync
pub fn isValidMp3(data: []const u8) bool {
    if (data.len < 4) return false;

    // Check for ID3v2 header: "ID3"
    if (data.len >= 10 and std.mem.eql(u8, data[0..3], "ID3")) {
        return true;
    }

    // Check for MP3 frame sync: 0xFF 0xFB, 0xFF 0xFA, 0xFF 0xF3, 0xFF 0xF2
    // Or more generally: 0xFF followed by 0xE0-0xFF
    if (data[0] == 0xFF and (data[1] & 0xE0) == 0xE0) {
        return true;
    }

    // Sometimes MP3 files have garbage at the start, scan first 4KB for frame sync
    const scan_len = @min(data.len - 1, 4096);
    for (0..scan_len) |i| {
        if (data[i] == 0xFF and (data[i + 1] & 0xE0) == 0xE0) {
            return true;
        }
    }

    return false;
}

/// Extract title from ID3v2 tag or filename
pub fn extractTitle(data: []const u8, filename: []const u8) []const u8 {
    // Try to extract from ID3v2 (simplified - just use filename for now)
    // Full ID3 parsing would require more complex logic
    _ = data;

    // Remove extension from filename
    if (std.mem.lastIndexOf(u8, filename, ".")) |dot| {
        return filename[0..dot];
    }
    return filename;
}

/// Generate unique filename for uploaded file
pub fn generateUniqueFilename(allocator: std.mem.Allocator, original: []const u8) ![]u8 {
    // Add timestamp to make unique
    const timestamp = std.time.timestamp();

    // Get extension
    const ext = if (std.mem.lastIndexOf(u8, original, ".")) |dot|
        original[dot..]
    else
        ".mp3";

    // Get basename without extension
    const basename_end = if (std.mem.lastIndexOf(u8, original, ".")) |dot| dot else original.len;
    const basename = original[0..basename_end];

    // Convert timestamp to string first to know exact length
    var ts_buf: [20]u8 = undefined;
    const ts_str = std.fmt.bufPrint(&ts_buf, "{d}", .{timestamp}) catch "0";

    // Allocate exact size: basename + '_' + timestamp + ext
    const total_len = basename.len + 1 + ts_str.len + ext.len;
    const result = try allocator.alloc(u8, total_len);

    var pos: usize = 0;
    @memcpy(result[pos..][0..basename.len], basename);
    pos += basename.len;
    result[pos] = '_';
    pos += 1;
    @memcpy(result[pos..][0..ts_str.len], ts_str);
    pos += ts_str.len;
    @memcpy(result[pos..][0..ext.len], ext);

    return result;
}

/// Sanitize filename for safe filesystem storage
pub fn sanitizeFilename(allocator: std.mem.Allocator, filename: []const u8) ![]u8 {
    // First pass: count output size
    var output_len: usize = 0;
    for (filename) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_' or ch == '.') {
            output_len += 1;
        } else if (ch == ' ') {
            output_len += 1;
        }
    }

    // Check if we need to add .mp3 extension
    var needs_ext = true;
    if (output_len >= 4) {
        var temp_result: [256]u8 = undefined;
        var temp_pos: usize = 0;
        for (filename) |ch| {
            if (temp_pos < temp_result.len) {
                if (std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_' or ch == '.') {
                    temp_result[temp_pos] = ch;
                    temp_pos += 1;
                } else if (ch == ' ') {
                    temp_result[temp_pos] = '_';
                    temp_pos += 1;
                }
            }
        }
        if (std.mem.endsWith(u8, temp_result[0..temp_pos], ".mp3")) {
            needs_ext = false;
        }
    }

    if (needs_ext) output_len += 4;

    // Allocate exact size
    const result = try allocator.alloc(u8, output_len);
    var pos: usize = 0;

    for (filename) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_' or ch == '.') {
            result[pos] = ch;
            pos += 1;
        } else if (ch == ' ') {
            result[pos] = '_';
            pos += 1;
        }
    }

    if (needs_ext) {
        @memcpy(result[pos..][0..4], ".mp3");
    }

    return result;
}

test "isValidMp3 with ID3v2" {
    const data = "ID3" ++ [_]u8{0} ** 7 ++ "extra data";
    try std.testing.expect(isValidMp3(data));
}

test "isValidMp3 with frame sync" {
    const data = [_]u8{ 0xFF, 0xFB, 0x90, 0x00 };
    try std.testing.expect(isValidMp3(&data));
}

test "isValidMp3 invalid" {
    const data = "not an mp3 file";
    try std.testing.expect(!isValidMp3(data));
}

test "sanitizeFilename" {
    const allocator = std.testing.allocator;
    const result = try sanitizeFilename(allocator, "My Song (2024).mp3");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("My_Song_2024.mp3", result);
}
