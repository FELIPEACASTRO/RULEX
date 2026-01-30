package com.rulex.core.approval.port;

public interface SecurityContextPort {

  String getCurrentUsernameOrSystem();

  String getCurrentClientIp();

  boolean isAdmin();
}
