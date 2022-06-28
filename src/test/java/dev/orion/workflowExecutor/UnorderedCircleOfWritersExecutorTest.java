package dev.orion.workflowExecutor;

import dev.orion.broker.producer.DocumentUpdateProducer;
import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.entity.*;
import dev.orion.entity.step_type.UnorderedCircleOfWriters;
import dev.orion.fixture.DocumentFixture;
import dev.orion.services.interfaces.DocumentService;
import dev.orion.services.interfaces.GroupService;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import io.quarkus.test.junit.mockito.InjectSpy;
import lombok.val;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.junit.jupiter.api.*;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import java.util.*;

import static dev.orion.fixture.ActivityFixture.generateActivity;
import static dev.orion.fixture.UserFixture.createParticipants;
import static dev.orion.fixture.UserFixture.generateUser;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.*;

@QuarkusTest
public class UnorderedCircleOfWritersExecutorTest {
    @Inject
    UnorderedCircleOfWriterStepExecutor testThis;

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

    private List<Document> usingDocuments;

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
        usingDocuments = usingGroup.getDocuments();
        val document = usingDocuments.get(0);
        given(Document.findByExternalId(document.getExternalId())).willReturn(Optional.of(document));
        DocumentFixture.mockFindByUserIdAndGroup(usingActivity);
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
        val document = usingDocuments.get(0);
        val userExecutor = usingParticipants.stream().findFirst().orElseThrow();
        testThis.execute(usingActivity, userExecutor, new UnorderedCircleOfWriters());

        then(documentService).should().moveParticipantToEditedList(any(Document.class), any(User.class));
        Assertions.assertTrue(document.getParticipantsThatEdited().contains(userExecutor));
        Assertions.assertFalse(document.getParticipantsAssigned().contains(userExecutor));
    }

    @Test
    @DisplayName("[executor] - (Single document) Should not call method when not find document with participant")
    public void testValidationOfNotFindDocumentWithParticipantMoveTheUserExecutorFromAssignedToEdited() {
        val document = usingDocuments.get(0);
        val userExecutor = usingParticipants.stream().findFirst().orElseThrow();
        document.removeParticipant(userExecutor);

        testThis.execute(usingActivity, userExecutor, new UnorderedCircleOfWriters());

        then(documentService).should(never()).moveParticipantToEditedList(any(Document.class), any(User.class));
    }


    @Test
    @DisplayName("[executor] - (Single document) Should increase round of document if every participant is in edited list")
    public void testIncreaseOfDocumentVersionIfFlowIsComplete() {
        val document = usingDocuments.get(0);
        val userExecutor = usingParticipants.stream().findFirst().orElseThrow();
        val participantsToEdit = new HashSet<>(document.getParticipantsAssigned());

        participantsToEdit.removeIf(user -> user.equals(userExecutor));
        document.getParticipantsAssigned().removeIf(user -> !user.equals(userExecutor));
        document.setParticipantsThatEdited(participantsToEdit);

        val expectedRounds = 2;
        usingStep.setRounds(expectedRounds);

        testThis.execute(usingActivity, userExecutor, usingStep);
        then(documentService).should().moveParticipantToEditedList(any(Document.class), any(User.class));
        Assertions.assertEquals(expectedRounds, document.getRounds());
    }

    @Test
    @DisplayName("[executor] - (Single document) Should let every participant perform document edition and add version at end")
    public void testAllParticipantsEditionAndIfAddNextRoundToDocument() {
        val document = usingDocuments.get(0);
        val expectedRounds = 2;
        usingStep.setRounds(expectedRounds);

        usingParticipants.forEach(user -> {
            testThis.execute(usingActivity, user, usingStep);
        });

        then(documentService).should(times(usingParticipants.size())).moveParticipantToEditedList(any(Document.class), any(User.class));
        Assertions.assertEquals(expectedRounds, document.getRounds());
    }

    @Test
    @DisplayName("[executor] - (Single document) Should reset the list after run every rounds")
    public void testRoundResetAfterLimitOfRoundIdAccepted() {
        val document = usingDocuments.get(0);
        val expectedRounds = 2;
        usingStep.setRounds(expectedRounds);

        usingParticipants.forEach(user -> {
            testThis.execute(usingActivity, user, usingStep);
        });
        usingParticipants.forEach(user -> {
            testThis.execute(usingActivity, user, usingStep);
        });

        val expectedExecutions = usingParticipants.size() * 2;
        then(documentService).should(times(expectedExecutions)).moveParticipantToEditedList(any(Document.class), any(User.class));
        then(documentService).should().moveAllUsersFromEditedToParticipantList(document.getExternalId());
        Assertions.assertEquals(expectedRounds, document.getRounds());
    }

    @Test
    @DisplayName("[validate] - (single document) Should validate if is user turn")
    @Disabled
    public void testValidationOfUserTurn() {
        testThis.validate(usingActivity, usingParticipants.stream().findFirst().orElseThrow(), new UnorderedCircleOfWriters());
    }
}
