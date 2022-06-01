package dev.orion.services;

import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.commom.constant.ActivityStages;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.GroupActivity;
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
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

    @Spy
    MockedStatic<GroupActivity> groupActivityMockedStatic;

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

        BDDMockito.then(document).should().addParticipant(user);
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

        BDDMockito.then(document).should().addParticipant(user);
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
        Assertions.assertEquals(MessageFormat.format("There are {0} users that are already in another group", 1), errorMessage);
        Assertions.assertNotEquals(user.getGroupActivity(), groupDestination);
    }

    @Test
    @DisplayName("Should not let add participant if group is full")
    public void testAddToFullGroupValidation() {
        val activity = new Activity();
        val user = UserFixture.generateUser();
        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val document = spy(documentService.createDocument(UUID.randomUUID(), "", Set.of(user)));

        val group = groupService.createGroup(activity);
        groupService.addUserToGroup(group, user, document);

        val secondUser = UserFixture.generateUser();
        activity.addParticipant(secondUser);

        String message = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            groupService.addUserToGroup(group, secondUser, document);
        }).getMessage();

        String expectedErrorMessage = MessageFormat.format("There are {0} users that can''t be placed on group {1} because its above the capacity", 1, group.getUuid());
        Assertions.assertEquals(expectedErrorMessage, message);
        BDDMockito.then(document).should(atMostOnce()).addParticipant(any());
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
                .format("There are {0} users that can''t be placed on group {1} because it not belongs to activity {2}",
                        1, group.getUuid(), activity.uuid);
        Assertions.assertEquals(expectedErrorMessage, message);
    }


//  GROUP REMOVE USER FROM GROUP
    @Test
    @DisplayName("Should remove user from group")
    public void testRemoveUser() {
        val activity = new Activity();
        val user = UserFixture.generateUser();
        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val document = spy(documentService.createDocument(UUID.randomUUID(), "", Set.of(user)));
        val group = spy(groupService.createGroup(activity));
        groupService.addUserToGroup(group, user, document);

        Assertions.assertTrue(document.getParticipantsAssigned().contains(user));
        Assertions.assertTrue(group.getParticipants().contains(user));

        groupService.removeUserFromGroup(activity, user);
        Assertions.assertFalse(document.getParticipantsAssigned().contains(user));
        Assertions.assertFalse(group.getParticipants().contains(user));

        BDDMockito.then(document).should().removeParticipant(user);
        BDDMockito.then(group).should().removeParticipant(user);
    }

    @Test
    @DisplayName("Should delete group after last user is removed")
    public void testEmptyGroupAfterRemove() {
        val activity = new Activity();
        val user = UserFixture.generateUser();
        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val document = documentService.createDocument(UUID.randomUUID(), "", Set.of(user));
        val group = groupService.createGroup(activity);
        groupService.addUserToGroup(group, user, document);
        groupService.removeUserFromGroup(activity, user);

        Assertions.assertFalse(activity.getGroupActivities().contains(group));

//  @TODO This test is false positive. Try to find some scenarios to verify if the Group was really deleted.
        groupActivityMockedStatic.verify(() -> GroupActivity.delete((String) any(), (Object) any()));

        Assertions.assertEquals(0, GroupActivity.find("uuid", group.getUuid()).count());
        Assertions.assertNull(document.getGroupActivity());
    }

//  GROUP transferUserToGroup
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
    @Disabled
    public void testTransferUserToFullGroup() {
        throw new NotImplementedYetException();
    }

    //    Capacity scenarios
    @Test
    @DisplayName("Should create a group with list same capacity number of activity participants")
    public void testGroupCapacityAsSameActivityParticipantNumber() {
        val activity = new Activity();
        val user = UserFixture.generateUser();
        val user1 = UserFixture.generateUser();
        val user2 = UserFixture.generateUser();

        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        activity.addParticipant(user1);
        activity.addParticipant(user2);
        activity.persistAndFlush();

        val group = groupService.createGroup(activity, Set.of(new User[]{user, user1, user2}));
        group.persist();

        Assertions.assertEquals(3, group.getCapacity());
        Assertions.assertEquals(activity.getUserList().size(), group.getCapacity());
    }

    @Test
    @DisplayName("Should create a group with capacity with same number of provided user list")
    public void testGroupCapacityAsSameProvidedUserList() {
        val activity = new Activity();
        val author = UserFixture.generateUser();

        var users = generateSetUsers();
        activity.getUserList().addAll(users);
        injectUserInActivity(activity, author);
        injectWorkflowInActivity(activity);

        activity.persist();

        val group = groupService.createGroup(activity, users);

        Assertions.assertEquals(users.size(), group.getParticipants().size());
        Assertions.assertEquals(users.size(), group.getCapacity());
    }

    @Test
    @DisplayName("Should change capacity of group")
    public void testChangeGroupCapacity() {
        val activity = new Activity();
        val author = UserFixture.generateUser();

        injectUserInActivity(activity, author);
        injectWorkflowInActivity(activity);

        val users = generateSetUsers();
        activity.getUserList().addAll(users);

        users.add(author);
        val group = groupService.createGroup(activity, users);
        group.persist();

        val newCapacity = users.size();
        groupService.changeGroupCapacity(activity, group, newCapacity);
        users.size(); group.getCapacity();
        Assertions.assertEquals(newCapacity, group.getCapacity());
    }

    @Test
    @DisplayName("Should not change capacity when the new quantity is less than the number of GROUP participants")
    public void testChangeGroupCapacityBelowParticipantsNumberValidation() {
        val activity = new Activity();
        val user = UserFixture.generateUser();

        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val users = generateSetUsers();
        activity.getUserList().addAll(users);

        val group = groupService.createGroup(activity, users);
        group.persist();
        val groupCapacityHolder = group.getCapacity();

        val newCapacity = 2;
        val errorMessage = Assertions.assertThrows(IllegalArgumentException.class, () -> {
            groupService.changeGroupCapacity(activity, group, newCapacity);
        }).getMessage();

        String expectedMessage = MessageFormat.format("Capacity {0} is less than the number of group {1} participants ({2})", newCapacity, group.getUuid(), group.getParticipants().size());
        Assertions.assertEquals(expectedMessage, errorMessage);
        Assertions.assertEquals(groupCapacityHolder, group.getCapacity());
    }

    @Test
    @DisplayName("Should not change capacity when the new quantity is higher than the number of ACTIVITY participants")
    public void testChangeGroupCapacityAboveOfActivityParticipants() {
        val activity = new Activity();
        val user = UserFixture.generateUser();

        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val users = generateSetUsers();
        activity.getUserList().addAll(users);
        activity.persist();

        val group = groupService.createGroup(activity, users);
        val groupCapacityHolder = group.getCapacity();

        val overCapacity = activity.getUserList().size() + 1;

        val errorMessage = Assertions.assertThrows(IllegalArgumentException.class, () -> {
            groupService.changeGroupCapacity(activity, group, overCapacity);
        }).getMessage();

        String expectedMessage = MessageFormat.format("Capacity {0} is more than the number of activity {1} participants ({2})", overCapacity, activity.getUuid(), activity.getUserList().size());
        Assertions.assertEquals(expectedMessage, errorMessage);
        Assertions.assertEquals(groupCapacityHolder, group.getCapacity());
    }


//    Document scenarios
//    @TODO Create document implementation first
    @Test
    @DisplayName("Should add document when create a group with list")
    public void testAddDocumentAndUsers() {
        val activity = new Activity();
        val user = UserFixture.generateUser();

        injectUserInActivity(activity, user);
        injectWorkflowInActivity(activity);

        val users = generateSetUsers();
        activity.getUserList().addAll(users);
        activity.persist();

        val group = groupService.createGroup(activity, users);

        Assertions.assertFalse(group.getDocuments().isEmpty());
    }

    private Set<User> generateSetUsers() {
        return Stream.of(new User[]{
                UserFixture.generateUser(), UserFixture.generateUser(),
                UserFixture.generateUser(), UserFixture.generateUser(),
                UserFixture.generateUser(), UserFixture.generateUser()}).collect(Collectors.toSet());
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
}
