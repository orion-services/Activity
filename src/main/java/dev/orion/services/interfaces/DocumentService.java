package dev.orion.services.interfaces;

import dev.orion.entity.Document;
import dev.orion.entity.User;

import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public interface DocumentService {
    Optional<Document> editContent(String content, UUID activityUuid, String externalUserId);
    Document createDocument(UUID uuid, String initialContent, Set<User> editors);
}
