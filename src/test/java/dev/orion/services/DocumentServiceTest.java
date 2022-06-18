package dev.orion.services;

import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentRequest;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.fixture.UserFixture;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import net.datafaker.Faker;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.junit.jupiter.api.*;
import org.mockito.*;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;

import static org.mockito.Mockito.*;

@QuarkusTest
@Transactional
public class DocumentServiceTest {

    @InjectMock
    @RestClient
    DocumentClient documentClient;

    @Inject
    DocumentServiceImpl testThis;

//    @InjectMock
//    Session session;

    @Captor
    ArgumentCaptor<CreateDocumentRequest> createDocumentRequestArgumentCaptor;

    String defaultIdResponse = UUID.randomUUID().toString();

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);

        val createDocumentResponse = new CreateDocumentResponse(
                defaultIdResponse,
                Faker.instance().backToTheFuture().quote(),
                UUID.randomUUID().toString());

        BDDMockito.given(documentClient.createDocument(any())).willReturn(createDocumentResponse);
    }

//    Test document Creation
    @Test
    @DisplayName("[createDocument] Should create document and assign to group")
    public void testDocumentCreation() {
        val user = UserFixture.generateUser();
        user.persist();
        val initialText = Faker.instance().yoda().quote();

        val document = testThis.createDocument(UUID.randomUUID(), initialText, new HashSet<>(List.of(user)));

        Assertions.assertNull(document.getGroupActivity());
        Assertions.assertEquals(defaultIdResponse, document.getExternalId());
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
        Assertions.assertEquals(createDocumentRequestArgumentCaptor.getValue().getOwnerId(), requestUuid);
        Assertions.assertEquals(createDocumentRequestArgumentCaptor.getValue().getInitialText(), initialText);
    }

//    Test document edition
    @Test
    @DisplayName("Should put content on the queue when call editContent")
    @Disabled
    public void test() {

    }
}
