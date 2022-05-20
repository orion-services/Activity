package dev.orion.services;

import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.commom.enums.ActivityStages;
import dev.orion.commom.exceptions.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.User;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.fixture.UserFixture;
import dev.orion.fixture.WorkflowFixture;
import dev.orion.services.interfaces.DocumentService;
import dev.orion.services.interfaces.GroupService;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import net.datafaker.Faker;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.jboss.resteasy.spi.NotImplementedYetException;
import org.junit.jupiter.api.*;
import org.mockito.BDDMockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.mockito.Mockito.*;

@QuarkusTest
@Transactional
public class GroupActivityServiceTest {

    @Inject
    GroupService groupService;

    @InjectMock
    @RestClient
    DocumentClient documentClient;

    @Inject
    DocumentService documentService;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        val createDocumentResponse = new CreateDocumentResponse(
                UUID.randomUUID().toString(),
                Faker.instance().backToTheFuture().quote(),
                UUID.randomUUID().toString());

        BDDMockito.given(documentClient.createDocument(any())).willReturn(createDocumentResponse);
    }

    //    User scenarios tests
    @Test
    @DisplayName("Should add user to the group")
    public void testAddToTheGroup() {
        val activity = new Activity();
        val user = UserFixture.generateUser();

        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val document = spy(documentService.createDocument(UUID.randomUUID(), "", Set.of(user)));

        val group = groupService.createGroup(activity);
        val spyGroup = spy(group);
        activity.groupActivities.add(group);

        groupService.addUserToGroup(spyGroup, user, document);

        Assertions.assertEquals(group.getParticipants().size(), 1);
        Assertions.assertEquals(user.getGroupActivity(), spyGroup);
        Assertions.assertTrue(document.getParticipantsAssigned().contains(user));
        Assertions.assertTrue(group.getDocuments().contains(document));

        BDDMockito.then(document).should().assignParticipant(user);
        BDDMockito.then(spyGroup).should().addParticipant(user);
        BDDMockito.then(spyGroup).should().addDocument(document);
    }

    @Test
    @DisplayName("Should add user to the group and not re-add an already added document")
    public void testAddToTheGroupWithSameDocument() {
        val activity = new Activity();
        val user = UserFixture.generateUser();
        user.persist();

        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val document = spy(documentService.createDocument(UUID.randomUUID(), "", Set.of(user)));

        val group = groupService.createGroup(activity);
        val spyGroup = spy(group);
        activity.groupActivities.add(group);
        group.addDocument(document);
        document.setGroupActivity(group);

        groupService.addUserToGroup(spyGroup, user, document);

        BDDMockito.then(document).should().assignParticipant(user);
        BDDMockito.then(spyGroup).should().addParticipant(user);
        BDDMockito.then(spyGroup).should(never()).addDocument(document);
    }

    @Test
    @DisplayName("Should throw exception when try to add an user that is already in another group")
    public void testAddUserInMoreThanOneGroup() {
        val activity = new Activity();
        val user = UserFixture.generateUser();
        user.persist();

        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val document = spy(documentService.createDocument(UUID.randomUUID(), "", Set.of(user)));

        val groupOrigin = groupService.createGroup(activity);
        val groupDestination = groupService.createGroup(activity);

        activity.groupActivities.add(groupOrigin);
        activity.groupActivities.add(groupDestination);

        groupService.addUserToGroup(groupOrigin, user, document);
        String errorMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            groupService.addUserToGroup(groupDestination, user, document);
        }).getMessage();
        Assertions.assertEquals(MessageFormat.format("The user {0} is already on another group", user.id), errorMessage);
        Assertions.assertNotEquals(user.getGroupActivity(), groupDestination);
    }

    @Test
    @DisplayName("Should not let add participant if group is full")
    public void testAddToFullGroupValidation() {
        val activity = new Activity();
        val user = UserFixture.generateUser();
        activity.userList.add(user);
        activity.setCreatedBy(user);

        injectWorkflowInActivity(activity);

        val document = spy(documentService.createDocument(UUID.randomUUID(), "", Set.of(user)));

        val group = groupService.createGroup(activity);
        groupService.addUserToGroup(group, user, document);
        val secondUser = UserFixture.generateUser();

        activity.groupActivities.add(group);

        String message = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            groupService.addUserToGroup(group, secondUser, document);
        }).getMessage();

        String expectedErrorMessage = MessageFormat.format("The user {0} can't be placed on group {1} because its is full", secondUser.id, group.id);
        Assertions.assertEquals(expectedErrorMessage, message);
        BDDMockito.then(document).should(atMostOnce()).assignParticipant(any());
    }

    @Test
    @DisplayName("Should not let add participant if they not belongs to same activity that owns group")
    public void testAddUserThatNotBelongToActivityOwnerValidation() {
        val activity = new Activity();
        val mainUser = UserFixture.generateUser();
        val document = spy(new Document());

        val group = groupService.createGroup(activity);
        group.setCapacity(2);

        activity.groupActivities.add(group);

        String message = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            groupService.addUserToGroup(group, mainUser, document);
        }).getMessage();
        String expectedErrorMessage = MessageFormat
                .format("The user {0} can't be placed on group {1} because it not belongs to activity {2}",
                        mainUser.id, group.id, activity.uuid);
        Assertions.assertEquals(expectedErrorMessage, message);
    }

    private void injectWorkflowInActivity(Activity activity) {
        val workflow = WorkflowFixture.generateWorkflow(
                List.of(WorkflowFixture.generateStage(
                        ActivityStages.PRE, List.of(new CircleOfWriters()))));

        activity.setWorkflow(workflow);
    }

    private void injectUserInActivity(Activity activity, User user) {
        activity.addParticipant(user);
        activity.setCreatedBy(user);
    }

    @Test
    @DisplayName("Should transfer an user between groups")
    @Disabled
    public void testTransferUserBetweenGroups() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should not transfer an user if it's not present in activity")
    @Disabled
    public void testTransferUserNotBelongingToActivityBetweenGroupsValidation() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should delete group from activity if there's no more users in group")
    @Disabled
    public void testTransferLastUserFromGroup() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should not let transfer from group if destination group is full")
    public void testTransferUserToFullGroup() {
        throw new NotImplementedYetException();
    }

    //    Capacity scenarios
    @Test
    @DisplayName("Should create a group with capacity same number of activity participants")
    @Disabled
    public void testGroupCapacityAsSameActivityParticipantNumber() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should create a group with same number of provided user list")
    @Disabled
    public void testGroupCapacityAsSameProvidedUserList() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should change capacity of group")
    @Disabled
    public void testChangeGroupCapacity() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should not change capacity when the new quantity is greater than number of participants")
    @Disabled
    public void testChangeGroupCapacityBelowParticipantsNumberValidation() {
        throw new NotImplementedYetException();
    }

    //    Document scenarios
//    @TODO Create document implementation first
    @Test
    @DisplayName("Should add document and users whenever is possible")
    @Disabled
    public void testAddDocumentAndUsers() {
        throw new NotImplementedYetException();
    }
}
