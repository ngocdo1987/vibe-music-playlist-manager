# Build stage
FROM alpine:3.19 AS builder

# Install Zig and build dependencies
RUN apk add --no-cache \
    wget \
    xz \
    sqlite-dev \
    musl-dev

# Download and install Zig
RUN wget https://ziglang.org/builds/zig-linux-x86_64-0.15.0-dev.559+d10bd5756.tar.xz \
    && tar -xf zig-linux-x86_64-0.15.0-dev.559+d10bd5756.tar.xz \
    && mv zig-linux-x86_64-0.15.0-dev.559+d10bd5756 /opt/zig \
    && rm zig-linux-x86_64-0.15.0-dev.559+d10bd5756.tar.xz

ENV PATH="/opt/zig:${PATH}"

# Copy source code
WORKDIR /app
COPY . .

# Build the application
RUN zig build -Doptimize=ReleaseSafe

# Runtime stage
FROM alpine:3.19

# Install runtime dependencies
RUN apk add --no-cache sqlite-libs

# Create non-root user
RUN addgroup -S music && adduser -S music -G music

# Create app directory
WORKDIR /app

# Copy binary and required files
COPY --from=builder /app/zig-out/bin/music-manager /app/
COPY --from=builder /app/public /app/public/

# Create data directory for database
RUN mkdir -p /app/data /app/public/mp3 \
    && chown -R music:music /app

# Use non-root user
USER music

# Expose port
EXPOSE 8080

# Set environment variables
ENV ADMIN_USERNAME=admin
ENV ADMIN_PASSWORD=changeme

# Run the application
CMD ["./music-manager"]
