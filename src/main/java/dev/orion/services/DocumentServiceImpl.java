package dev.orion.services;

import dev.orion.broker.producer.DocumentUpdateProducer;
import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentRequest;
import dev.orion.entity.Document;
import dev.orion.entity.User;
import dev.orion.services.interfaces.DocumentService;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.jboss.logging.Logger;
import org.jboss.resteasy.spi.NotImplementedYetException;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.NotFoundException;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@ApplicationScoped
public class DocumentServiceImpl implements DocumentService {

    @LoggerName("DocumentServiceImpl")
    Logger logger;
    @RestClient
    DocumentClient documentClient;

    @Inject
    DocumentUpdateProducer documentUpdateProducer;

    @Override
    public void editContent(Document document, String content, String externalUserId) {
        throw new NotImplementedYetException();
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

    @Override
    public void moveParticipantToEditedList(Document document, User user) {
        val isRemoved = document.getParticipantsAssigned().remove(user);
        if (isRemoved) {
            document.addParticipantThatEdited(user);
            logger.infov("User {0} has been moved from participants to edited list in document {1}", user.getExternalId(), document.getExternalId());
            return;
        }

        logger.warnv("User {0} not found in document {1}", user.getExternalId(), document.getExternalId());
    }

    @Override
    public void moveAllUsersFromEditedToParticipantList(String documentExternalId) {
        val document = Document
                .findByExternalId(documentExternalId)
                .orElseThrow(() -> {
                    throw new NotFoundException("Document with ID "+ documentExternalId + " not found");
                });

        val participantsThatEdited = new HashSet<>(document.getParticipantsThatEdited());

        document.setParticipantsAssigned(participantsThatEdited);
        document.setParticipantsThatEdited(new HashSet<>());
        logger.infov("Users has been moved from edited list to participant list in document {1}", document.getExternalId());

    }
}
