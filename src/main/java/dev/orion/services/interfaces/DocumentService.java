package dev.orion.services.interfaces;

import dev.orion.data.entity.Document;

import java.util.Optional;
import java.util.UUID;

public interface DocumentService {
    Optional<Document> editContent(String content, UUID activityUuid, String externalUserId);

}
