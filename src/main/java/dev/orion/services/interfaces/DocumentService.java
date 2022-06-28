package dev.orion.services.interfaces;

import dev.orion.entity.Document;
import dev.orion.entity.User;

import java.util.Set;
import java.util.UUID;

public interface DocumentService {

    void editContent(Document document, String content, String externalUserId);
    Document createDocument(UUID uuid, String initialContent, Set<User> editors);

    void moveParticipantToEditedList(Document document, User user);

    void moveAllUsersFromEditedToParticipantList(String documentExternalId);
}
