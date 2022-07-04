package dev.orion.services;

import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentRequest;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.fixture.UserFixture;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import net.datafaker.Faker;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.BDDMockito;
import org.mockito.Captor;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.NotFoundException;
import java.text.MessageFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.any;

@QuarkusTest
@Transactional
public class DocumentServiceTest {
    @Inject
    DocumentServiceImpl testThis;

    @InjectMock
    @RestClient
    DocumentClient documentClient;

    @InjectMock
    Session session;

    @Captor
    ArgumentCaptor<CreateDocumentRequest> createDocumentRequestArgumentCaptor;

    UUID documentExternalId = UUID.randomUUID();

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        PanacheMock.mock(Document.class);

        val createDocumentResponse = new CreateDocumentResponse(
                documentExternalId.toString(),
                Faker.instance().backToTheFuture().quote());

        given(documentClient.createDocument(any())).willReturn(createDocumentResponse);
    }

    @Test
    @DisplayName("[createDocument] Should create document")
    public void testDocumentCreation() {
        val user = UserFixture.generateUser();
        user.persist();
        val initialText = Faker.instance().yoda().quote();

        val document = testThis.createDocument(UUID.randomUUID(), initialText, new HashSet<>(List.of(user)));

        Assertions.assertEquals(documentExternalId.toString(), document.getExternalId());
        Assertions.assertTrue(document.getParticipantsAssigned().contains(user));
    }

    @Test
    @DisplayName("[createDocument] Should create document and have side effects")
    public void testDocumentCreationSideEffects() {
        val user = UserFixture.generateUser();
        user.persist();
        val initialText = Faker.instance().yoda().quote();
        val requestUuid = UUID.randomUUID();

        testThis.createDocument(requestUuid, initialText, new HashSet<>(List.of(user)));

        BDDMockito.then(documentClient).should().createDocument(createDocumentRequestArgumentCaptor.capture());
        Assertions.assertEquals(createDocumentRequestArgumentCaptor.getValue().getId(), requestUuid);
        Assertions.assertEquals(createDocumentRequestArgumentCaptor.getValue().getInitialText(), initialText);
    }

    @Test
    @DisplayName("[moveParticipantToEdited] - Should exchange user from participant to edited list")
    public void testMoveParticipantFromParticipantToEditedList() {
        val user = UserFixture.generateUser();
        user.persist();
        val requestUuid = UUID.randomUUID();

        val document = testThis.createDocument(requestUuid, "", new HashSet<>(List.of(user)));
        testThis.moveParticipantToEditedList(document, user);

        Assertions.assertTrue(document.getParticipantsAssigned().isEmpty());
        Assertions.assertTrue(document.getParticipantsThatEdited().contains(user));
    }

    @Test
    @DisplayName("[moveParticipantToEdited] - Should not exchange user from participant to edited list if not present in participants")
    public void testMoveParticipantFromParticipantToEditedListNotFoundPrevention() {
        val user = UserFixture.generateUser();
        val secondUser = UserFixture.generateUser();
        val requestUuid = UUID.randomUUID();

        val document = testThis.createDocument(requestUuid, "", new HashSet<>(List.of(user)));

        testThis.moveParticipantToEditedList(document, secondUser);

        Assertions.assertFalse(document.getParticipantsAssigned().contains(secondUser));
        Assertions.assertFalse(document.getParticipantsThatEdited().contains(user));
    }

    @Test
    @DisplayName("[moveAllUsersFromEditedToParticipantList] - Should move all users from edited list to participants")
    public void testMoveAllUsersFromEditedToParticipantList() {
        val participants = UserFixture.createParticipants(new Activity(), 5);
        val document = testThis.createDocument(documentExternalId, "", participants);
        given(Document.findByExternalId(documentExternalId.toString())).willReturn(Optional.of(document));

        val participantsToEdit = new HashSet<>(document.getParticipantsAssigned());

        document.setParticipantsAssigned(new HashSet<>());
        document.setParticipantsThatEdited(participantsToEdit);

        testThis.moveAllUsersFromEditedToParticipantList(document.getExternalId());
        Assertions.assertTrue(document.getParticipantsThatEdited().isEmpty());
        Assertions.assertEquals(participants.size(), document.getParticipantsAssigned().size());
    }

    @Test
    @DisplayName("[moveAllUsersFromEditedToParticipantList] - Should skip the tentative if there is no document is found")
    public void testEmptyListWhenTryToMoveAllUsersFromEditedToParticipantList() {
        val participants = UserFixture.createParticipants(new Activity(), 5);
        val document = testThis.createDocument(documentExternalId, "", participants);
        given(Document.findByExternalId(documentExternalId.toString())).willReturn(Optional.empty());

        val participantsToEdit = new HashSet<>(document.getParticipantsAssigned());

        document.setParticipantsAssigned(new HashSet<>());
        document.setParticipantsThatEdited(participantsToEdit);

        val exceptionMessage = Assertions.assertThrows(NotFoundException.class, () -> {
            testThis.moveAllUsersFromEditedToParticipantList(document.getExternalId());
        }).getMessage();

        val expectedMessage = MessageFormat.format("Document with ID {0} not found", documentExternalId);
        Assertions.assertTrue(document.getParticipantsAssigned().isEmpty());
        Assertions.assertEquals(participants.size(), document.getParticipantsThatEdited().size());
        Assertions.assertEquals(expectedMessage, expectedMessage);
    }

    @Test
    @DisplayName("[editContent] Not implemented yet")
    public void testNotImplementedDisconnectUserFromActivity() {
        Assertions.assertThrows(RuntimeException.class, () -> testThis.editContent(new Document(), Faker.instance().backToTheFuture().quote(),documentExternalId.toString()));
    }
}
