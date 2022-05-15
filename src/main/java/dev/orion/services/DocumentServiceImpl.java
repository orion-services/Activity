package dev.orion.services;

import dev.orion.entity.Document;
import dev.orion.services.interfaces.DocumentService;

import java.util.Optional;
import java.util.UUID;

public class DocumentServiceImpl implements DocumentService {
    @Override
    public Optional<Document> editContent(String content, UUID activityUuid, String externalUserId) {
        return Optional.empty();
    }
}
