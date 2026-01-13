-- V19: Access Log Table for HTTP Access Auditing
-- This table stores HTTP access events for security auditing and compliance

CREATE TABLE IF NOT EXISTS access_logs (
    id BIGSERIAL PRIMARY KEY,
    username VARCHAR(100),
    event_type VARCHAR(50) NOT NULL,
    http_method VARCHAR(10),
    request_uri VARCHAR(500),
    response_status INTEGER,
    source_ip VARCHAR(45),
    user_agent VARCHAR(500),
    session_id VARCHAR(100),
    details TEXT,
    duration_ms BIGINT,
    timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for common query patterns
CREATE INDEX idx_access_username ON access_logs(username);
CREATE INDEX idx_access_event_type ON access_logs(event_type);
CREATE INDEX idx_access_timestamp ON access_logs(timestamp);
CREATE INDEX idx_access_source_ip ON access_logs(source_ip);

-- Composite index for security monitoring queries
CREATE INDEX idx_access_security ON access_logs(username, event_type, timestamp);

-- Comment on table and columns
COMMENT ON TABLE access_logs IS 'HTTP access events for security auditing';
COMMENT ON COLUMN access_logs.username IS 'Authenticated username (null for anonymous)';
COMMENT ON COLUMN access_logs.event_type IS 'Type of access event (LOGIN_SUCCESS, LOGIN_FAILURE, API_ACCESS, etc.)';
COMMENT ON COLUMN access_logs.http_method IS 'HTTP method (GET, POST, PUT, DELETE, etc.)';
COMMENT ON COLUMN access_logs.request_uri IS 'Request URI path';
COMMENT ON COLUMN access_logs.response_status IS 'HTTP response status code';
COMMENT ON COLUMN access_logs.source_ip IS 'Client IP address (supports IPv6)';
COMMENT ON COLUMN access_logs.user_agent IS 'User-Agent header from request';
COMMENT ON COLUMN access_logs.session_id IS 'HTTP session ID if available';
COMMENT ON COLUMN access_logs.details IS 'Additional details in JSON format';
COMMENT ON COLUMN access_logs.duration_ms IS 'Request processing time in milliseconds';
COMMENT ON COLUMN access_logs.timestamp IS 'Timestamp of the access event';
