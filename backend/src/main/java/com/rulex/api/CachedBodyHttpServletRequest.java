package com.rulex.api;

import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletRequestWrapper;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

/**
 * HttpServletRequest wrapper that caches the body bytes so it can be read multiple times.
 *
 * <p>Implements a size limit to prevent memory DoS attacks.
 */
public class CachedBodyHttpServletRequest extends HttpServletRequestWrapper {

  /** Maximum body size in bytes (2MB default) */
  public static final int MAX_BODY_SIZE = 2 * 1024 * 1024;

  private final byte[] cachedBody;

  public CachedBodyHttpServletRequest(HttpServletRequest request) throws IOException {
    this(request, MAX_BODY_SIZE);
  }

  public CachedBodyHttpServletRequest(HttpServletRequest request, int maxBodySize)
      throws IOException {
    super(request);

    // Check Content-Length header first for early rejection
    int contentLength = request.getContentLength();
    if (contentLength > maxBodySize) {
      throw new PayloadTooLargeException(
          String.format("Request body too large: %d bytes (max: %d)", contentLength, maxBodySize));
    }

    // Read with limit
    try (InputStream is = request.getInputStream()) {
      this.cachedBody = readWithLimit(is, maxBodySize);
    }
  }

  /** Reads input stream with a size limit to prevent memory DoS. */
  private byte[] readWithLimit(InputStream is, int maxSize) throws IOException {
    ByteArrayOutputStream buffer = new ByteArrayOutputStream();
    byte[] chunk = new byte[8192];
    int bytesRead;
    int totalRead = 0;

    while ((bytesRead = is.read(chunk)) != -1) {
      totalRead += bytesRead;
      if (totalRead > maxSize) {
        throw new PayloadTooLargeException(
            String.format("Request body exceeds maximum size of %d bytes", maxSize));
      }
      buffer.write(chunk, 0, bytesRead);
    }

    return buffer.toByteArray();
  }

  public byte[] getCachedBody() {
    return cachedBody;
  }

  @Override
  public ServletInputStream getInputStream() {
    ByteArrayInputStream bais = new ByteArrayInputStream(cachedBody);
    return new ServletInputStream() {
      @Override
      public int read() {
        return bais.read();
      }

      @Override
      public boolean isFinished() {
        return bais.available() == 0;
      }

      @Override
      public boolean isReady() {
        return true;
      }

      @Override
      public void setReadListener(ReadListener readListener) {
        // no-op (sync)
      }
    };
  }

  @Override
  public BufferedReader getReader() {
    return new BufferedReader(
        new InputStreamReader(new ByteArrayInputStream(cachedBody), StandardCharsets.UTF_8));
  }

  /** Exception thrown when request body exceeds the maximum allowed size. */
  public static class PayloadTooLargeException extends IOException {
    private static final long serialVersionUID = 1L;

    public PayloadTooLargeException(String message) {
      super(message);
    }
  }
}
