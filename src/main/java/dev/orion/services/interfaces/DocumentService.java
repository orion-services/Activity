package dev.orion.services.interfaces;

import java.util.UUID;

public interface DocumentService {
    Boolean editContent(String content, UUID activityUuid, String externalUserId);

}
