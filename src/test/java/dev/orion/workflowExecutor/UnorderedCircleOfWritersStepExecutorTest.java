package dev.orion.workflowExecutor;

import dev.orion.broker.producer.DocumentUpdateProducer;
import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.GroupActivity;
import dev.orion.entity.User;
import dev.orion.entity.step_type.UnorderedCircleOfWriters;
import dev.orion.fixture.DocumentFixture;
import dev.orion.services.interfaces.DocumentService;
import dev.orion.services.interfaces.GroupService;
import dev.orion.workflowExecutor.impl.UnorderedCircleOfWritersStepExecutor;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import io.quarkus.test.junit.mockito.InjectSpy;
import lombok.val;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.*;

import static dev.orion.fixture.ActivityFixture.generateActivity;
import static dev.orion.fixture.UserFixture.createParticipants;
import static dev.orion.fixture.UserFixture.generateUser;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.*;

@QuarkusTest
public class UnorderedCircleOfWritersStepExecutorTest {
    @Inject
    UnorderedCircleOfWritersStepExecutor testThis;

    @InjectMock
    Session session;

    @InjectMock
    DocumentUpdateProducer documentUpdateProducer;

    @RestClient
    @InjectMock
    DocumentClient documentClient;

    @InjectSpy
    GroupService groupService;

    @InjectSpy
    DocumentService documentService;

    private User userCreator;

    private Activity usingActivity;

    private GroupActivity usingGroup;

    private LinkedHashSet<User> usingParticipants;

    private Document usingDocument;

    private UnorderedCircleOfWriters usingStep;

    @BeforeEach
    private void setup() {
        MockitoAnnotations.openMocks(this);
        PanacheMock.mock(Document.class);

        usingStep = new UnorderedCircleOfWriters();

        userCreator = generateUser();

        usingActivity = generateActivity(userCreator);

        usingParticipants = createParticipants(usingActivity, 5);
        given(documentClient.createDocument(any())).willReturn(new CreateDocumentResponse(UUID.randomUUID().toString(), ""));

        usingGroup = groupService.createGroup(usingActivity, usingParticipants);
        usingGroup.setUuid(UUID.randomUUID());
        usingDocument = usingGroup.getDocuments().get(0);
        DocumentFixture.mockFindByUserIdAndGroup(usingActivity);
        given(Document.findByExternalId(usingDocument.getExternalId())).willReturn(Optional.of(usingDocument));
    }

    @Test
    @DisplayName("[getStepRepresentation] - Should return the CircleOfWriters name")
    public void testGetStepRepresentationReturn() {
        val stepRepresentation = testThis.getStepRepresentation();

        Assertions.assertEquals(new UnorderedCircleOfWriters().getStepType(), stepRepresentation);
    }

    @Test
    @DisplayName("[executor] - (Single document) Should put the user that edited from participant to has edited")
    public void testMoveTheUserExecutorFromAssignedToEdited() {
        val userExecutor = usingParticipants.stream().findFirst().orElseThrow();
        testThis.execute(usingDocument, userExecutor, new UnorderedCircleOfWriters());

        then(documentService).should().moveParticipantToEditedList(any(Document.class), any(User.class));
        Assertions.assertTrue(usingDocument.getParticipantsThatEdited().contains(userExecutor));
        Assertions.assertFalse(usingDocument.getParticipantsAssigned().contains(userExecutor));
    }

    @Test
    @DisplayName("[executor] - (Single document) Should not call method when not find document with participant")
    public void testValidationOfNotFindDocumentWithParticipantMoveTheUserExecutorFromAssignedToEdited() {
        val userExecutor = usingParticipants.stream().findFirst().orElseThrow();
        usingDocument.removeParticipant(userExecutor);

        testThis.execute(usingDocument, userExecutor, new UnorderedCircleOfWriters());

        then(documentService).should(never()).moveParticipantToEditedList(any(Document.class), any(User.class));
    }

    @Test
    @DisplayName("[executor] - (Single document) Should throw when document is null")
    public void testNullValidationOfExecutor() {
        val userExecutor = usingParticipants.stream().findFirst().orElseThrow();
        usingDocument.removeParticipant(userExecutor);

        val exceptionMessage = Assertions.assertThrows(NotValidActionException.class, () -> {
            testThis.execute(null, userExecutor, new UnorderedCircleOfWriters());
        }).toString();
        val expectedMessage = MessageFormat.format("Step name: {0} has error: document must not be null", testThis.getStepRepresentation());

        Assertions.assertEquals(expectedMessage, exceptionMessage);
        then(documentService).should(never()).moveParticipantToEditedList(any(Document.class), any(User.class));
    }


    @Test
    @DisplayName("[executor] - (Single document) Should increase round of document if every participant is in edited list")
    public void testIncreaseOfDocumentVersionIfFlowIsComplete() {
        val userExecutor = usingParticipants.stream().findFirst().orElseThrow();
        val participantsToEdit = new HashSet<>(usingDocument.getParticipantsAssigned());

        participantsToEdit.removeIf(user -> user.equals(userExecutor));
        usingDocument.getParticipantsAssigned().removeIf(user -> !user.equals(userExecutor));
        usingDocument.setParticipantsThatEdited(participantsToEdit);

        val expectedRounds = 2;
        usingStep.setRounds(expectedRounds);

        testThis.execute(usingDocument, userExecutor, usingStep);
        then(documentService).should().moveParticipantToEditedList(any(Document.class), any(User.class));
        Assertions.assertEquals(expectedRounds, usingDocument.getRounds());
    }

    @Test
    @DisplayName("[executor] - (Single document) Should let every participant perform document edition and add version at end")
    public void testAllParticipantsEditionAndIfAddNextRoundToDocument() {
        val expectedRounds = 2;
        usingStep.setRounds(expectedRounds);

        usingParticipants.forEach(user -> {
            testThis.execute(usingDocument, user, usingStep);
        });

        then(documentService).should(times(usingParticipants.size())).moveParticipantToEditedList(any(Document.class), any(User.class));
        Assertions.assertEquals(expectedRounds, usingDocument.getRounds());
    }

    @Test
    @DisplayName("[executor] - (Single document) Should reset the list after run every rounds")
    public void testRoundResetAfterLimitOfRoundIdAccepted() {
        val expectedRounds = 2;
        usingStep.setRounds(expectedRounds);

        usingParticipants.forEach(user -> {
            testThis.execute(usingDocument, user, usingStep);
        });
        usingParticipants.forEach(user -> {
            testThis.execute(usingDocument, user, usingStep);
        });

        val expectedExecutions = usingParticipants.size() * 2;
        then(documentService).should(times(expectedExecutions)).moveParticipantToEditedList(any(Document.class), any(User.class));
        then(documentService).should().moveAllUsersFromEditedToParticipantList(usingDocument.getExternalId());
        Assertions.assertEquals(expectedRounds, usingDocument.getRounds());
    }

    @Test
    @DisplayName("[validate] - Should validate and let user execute activity")
    public void testValidationOfUserTurn() {
        testThis.validate(usingDocument, usingParticipants.stream().findFirst().orElseThrow(),usingStep);
    }

    @Test
    @DisplayName("[validate] - Should not let user edit document more than one time")
    public void testValidationOfUserThatHasAlreadyEditedDocument() {
        val executor = usingParticipants.stream().findFirst().orElseThrow();
        documentService.moveParticipantToEditedList(usingDocument, executor);
        val exceptionString = Assertions.assertThrows(NotValidActionException.class, () -> {
            testThis.validate(usingDocument, executor, usingStep);
        }).toString();

        val expectedMessage = MessageFormat.format("Step name: {0} has error: User {1} is not a participant in document {2}",
                testThis.getStepRepresentation(), executor.getExternalId(), usingDocument.getExternalId());
        Assertions.assertEquals(expectedMessage, exceptionString);
    }

    @Test
    @DisplayName("[validate] - Should verify if document is null")
    public void testValidationIfDocumentIsNull() {
        val executor = usingParticipants.stream().findFirst().orElseThrow();
        val exceptionString = Assertions.assertThrows(NotValidActionException.class, () -> {
            testThis.validate(null, executor, usingStep);
        }).toString();

        val expectedMessage = MessageFormat.format("Step name: {0} has error: document must not be null",
                testThis.getStepRepresentation());
        Assertions.assertEquals(expectedMessage, exceptionString);
    }

    @Test
    @DisplayName("[isFinished] - Should return true when task is finished")
    public void testFinishedStepReturn() {
        val spyDocument = spy(usingDocument);
        usingParticipants.forEach(user -> {
            testThis.execute(usingDocument, user, usingStep);
        });

        given(Document.findAllByGroupActivity(usingActivity.uuid)).willReturn(Arrays.asList(spyDocument));
        val stepFinished = testThis.isFinished(usingActivity, usingStep);
        then(spyDocument).should().getParticipantsAssigned();
        then(spyDocument).should().getRounds();
        Assertions.assertTrue(stepFinished);
    }

    @Test
    @DisplayName("[isFinished] - Should return true when task is finished")
    public void testFinishedStepListValidation() {
        val spyDocument = spy(usingDocument);
        usingParticipants.forEach(user -> {
            testThis.execute(usingDocument, user, usingStep);
        });

        given(Document.findAllByGroupActivity(UUID.randomUUID())).willReturn(Arrays.asList(spyDocument));
        val exceptionMessage = Assertions.assertThrows(NotValidActionException.class, () -> testThis.isFinished(usingActivity, usingStep)).toString();

        val expectedMessage = MessageFormat.format("Step name: {0} has error: document must not be null", usingStep.getStepType());
        Assertions.assertEquals(expectedMessage, exceptionMessage);
        then(spyDocument).should(never()).getParticipantsAssigned();
        then(spyDocument).should(never()).getRounds();
    }

    @Test
    @DisplayName("[isFinished] - Should return false when there is participants that has no edited")
    public void testFinishedStepReturnWhenThereIsUserToParticipate() {
        val spyDocument = spy(usingDocument);
        given(Document.findAllByGroupActivity(usingActivity.uuid)).willReturn(Arrays.asList(spyDocument));

        val stepFinished = testThis.isFinished(usingActivity, usingStep);

        then(spyDocument).should().getParticipantsAssigned();
        then(spyDocument).should(never()).getRounds();
        Assertions.assertFalse(stepFinished);
    }

    @Test
    @DisplayName("[isFinished] - Should return false when document round is less then configured in step")
    public void testFinishedStepReturnWhenThereIsMoreRoundsTo() {
        val spyDocument = spy(usingDocument);
        val expectedRounds = 2;
        usingStep.setRounds(expectedRounds);

        usingParticipants.forEach(user -> {
            testThis.execute(usingDocument, user, usingStep);
        });

        given(Document.findAllByGroupActivity(usingActivity.uuid)).willReturn(Arrays.asList(spyDocument));
        val stepFinished = testThis.isFinished(usingActivity, usingStep);
        then(spyDocument).should().getParticipantsAssigned();
        then(spyDocument).should().getRounds();
        Assertions.assertFalse(stepFinished);
    }
}
