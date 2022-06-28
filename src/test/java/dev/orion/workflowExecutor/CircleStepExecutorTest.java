package dev.orion.workflowExecutor;

import dev.orion.broker.producer.DocumentUpdateProducer;
import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.GroupActivity;
import dev.orion.entity.User;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.fixture.DocumentFixture;
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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.UUID;

import static dev.orion.fixture.ActivityFixture.generateActivity;
import static dev.orion.fixture.UserFixture.generateUser;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;

@QuarkusTest
@Disabled
public class CircleStepExecutorTest {
    @Inject
    CircleStepExecutor testThis;

    @InjectMock
    Session session;

    @InjectMock
    DocumentUpdateProducer documentUpdateProducer;

    @RestClient
    @InjectMock
    DocumentClient documentClient;

    @InjectSpy
    GroupService groupService;

    private User userCreator;

    private Activity usingActivity;

    private GroupActivity usingGroup;

    private LinkedHashSet<User> usingParticipants;

    private List<Document> usingDocuments;

    @BeforeEach
    private void setup() {
        MockitoAnnotations.openMocks(this);
        PanacheMock.mock(Document.class);

        userCreator = generateUser();

        usingActivity = generateActivity(userCreator);
        usingActivity.addParticipant(userCreator);

        usingParticipants = createParticipants(usingActivity);
        given(documentClient.createDocument(any())).willReturn(new CreateDocumentResponse(UUID.randomUUID().toString(), ""));

        usingGroup = groupService.createGroup(usingActivity, usingParticipants);
        usingDocuments = usingGroup.getDocuments();
        DocumentFixture.mockFindByUserIdAndGroup(usingActivity);
    }

    private LinkedHashSet<User> createParticipants(Activity activity) {
        val quantity =  5;
        val participants = new LinkedHashSet<User>();

        for (int i = 0; i < quantity; i++) {
            val user = generateUser();
            activity.addParticipant(user);
            participants.add(user);
        }

        return participants;
    }

    @Test
    @DisplayName("[getStepRepresentation] - Should return the CircleOfWriters name")
    public void testGetStepRepresentationReturn() {
        val stepRepresentation = testThis.getStepRepresentation();

        Assertions.assertEquals(new CircleOfWriters().getStepType(), stepRepresentation);
    }

    @Test
    @DisplayName("[validate] - (single document) Should validate if is user turn")
    public void testValidationOfUserTurn() {
        testThis.validate(usingActivity, usingParticipants.stream().findFirst().orElseThrow(), new CircleOfWriters());
    }
}
