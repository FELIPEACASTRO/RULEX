package com.rulex.homolog.port;

import java.util.UUID;

public interface ActorResolverPort {
  UUID resolveUserIdOrNull(String actorEmail);
}
