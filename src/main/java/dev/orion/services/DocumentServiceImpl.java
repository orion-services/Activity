package dev.orion.services;

import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentRequest;
import dev.orion.entity.Document;
import dev.orion.entity.User;
import dev.orion.services.interfaces.DocumentService;
import lombok.val;
import org.eclipse.microprofile.rest.client.inject.RestClient;

import javax.enterprise.context.ApplicationScoped;
import java.util.*;

@ApplicationScoped
public class DocumentServiceImpl implements DocumentService {
    @RestClient
    DocumentClient documentClient;

    @Override
    public Optional<Document> editContent(String content, UUID activityUuid, String externalUserId) {
        return Optional.empty();
    }

    @Override
    public Document createDocument(UUID uuid, String initialContent, Set<User> editors) {
        val document = new Document();
        val documentResponse = documentClient.createDocument(new CreateDocumentRequest(uuid, initialContent));

        document.setExternalId(documentResponse.getId());
        document.assignMultipleParticipants(editors);
        document.persist();

        return document;
    }
}
