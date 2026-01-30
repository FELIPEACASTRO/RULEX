package com.rulex.adapter.approval;

import com.rulex.core.approval.port.SecurityContextPort;
import com.rulex.service.SecurityContextService;
import org.springframework.stereotype.Component;

@Component
public class SecurityContextAdapter implements SecurityContextPort {

  private final SecurityContextService securityContextService;

  public SecurityContextAdapter(SecurityContextService securityContextService) {
    this.securityContextService = securityContextService;
  }

  @Override
  public String getCurrentUsernameOrSystem() {
    return securityContextService.getCurrentUsernameOrSystem();
  }

  @Override
  public String getCurrentClientIp() {
    return securityContextService.getCurrentClientIp();
  }

  @Override
  public boolean isAdmin() {
    return securityContextService.isAdmin();
  }
}
