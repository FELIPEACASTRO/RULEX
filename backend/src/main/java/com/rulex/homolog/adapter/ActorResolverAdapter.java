package com.rulex.homolog.adapter;

import com.rulex.homolog.port.ActorResolverPort;
import com.rulex.repository.homolog.UserRepository;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
public class ActorResolverAdapter implements ActorResolverPort {

  private final UserRepository userRepository;

  public ActorResolverAdapter(UserRepository userRepository) {
    this.userRepository = userRepository;
  }

  @Override
  public UUID resolveUserIdOrNull(String actorEmail) {
    if (actorEmail == null || actorEmail.isBlank()) {
      return null;
    }
    return userRepository.findByEmail(actorEmail.trim()).map(u -> u.getId()).orElse(null);
  }
}
