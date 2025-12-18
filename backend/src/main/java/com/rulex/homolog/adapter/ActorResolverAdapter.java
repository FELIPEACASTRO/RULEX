package com.rulex.homolog.adapter;

import com.rulex.homolog.ActorResolver;
import com.rulex.homolog.port.ActorResolverPort;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
public class ActorResolverAdapter implements ActorResolverPort {

  private final ActorResolver actorResolver;

  public ActorResolverAdapter(ActorResolver actorResolver) {
    this.actorResolver = actorResolver;
  }

  @Override
  public UUID resolveUserIdOrNull(String actorEmail) {
    return actorResolver.resolveUserIdOrNull(actorEmail);
  }
}
