package com.rulex.homolog;

import com.rulex.repository.homolog.UserRepository;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class ActorResolver {

    private final UserRepository userRepository;

    public ActorResolver(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public UUID resolveUserIdOrNull(String actorEmail) {
        if (actorEmail == null || actorEmail.isBlank()) {
            return null;
        }
        return userRepository.findByEmail(actorEmail.trim())
                .map(u -> u.getId())
                .orElse(null);
    }
}
