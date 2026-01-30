package com.rulex.core.engine.port;

public interface PayloadHashPort {

  String sha256Hex(byte[] payload);
}
