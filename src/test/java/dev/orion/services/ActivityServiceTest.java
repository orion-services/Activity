package dev.orion.services;

import dev.orion.broker.dto.DocumentUpdateDto;
import dev.orion.broker.producer.DocumentUpdateProducer;
import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.commom.constant.ActivityStage;
import dev.orion.commom.constant.UserRoles;
import dev.orion.commom.constant.UserStatus;
import dev.orion.commom.exception.InvalidActivityActionException;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.*;
import dev.orion.entity.step_type.UnorderedCircleOfWriters;
import dev.orion.fixture.ActivityFixture;
import dev.orion.fixture.DocumentFixture;
import dev.orion.fixture.UserFixture;
import dev.orion.fixture.WorkflowFixture;
import dev.orion.services.dto.ActivityExecutionDto;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.GroupService;
import dev.orion.services.interfaces.UserService;
import dev.orion.services.interfaces.WorkflowManageService;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import io.quarkus.test.junit.mockito.InjectSpy;
import lombok.SneakyThrows;
import lombok.val;
import net.datafaker.Faker;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.jboss.resteasy.spi.NotImplementedYetException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.BDDMockito;

import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.NotFoundException;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.*;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.BDDMockito.*;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;

@QuarkusTest
@Transactional
public class ActivityServiceTest {
    @Inject
    ActivityService testingThis;

    @InjectMock
    UserService userService;

    @InjectSpy
    GroupService groupService;

    @InjectMock
    DocumentUpdateProducer documentUpdateProducer;

    @InjectMock
    @RestClient
    DocumentClient documentClient;

    @InjectMock
    Session session;

    @InjectMock
    WorkflowManageService workflowManageService;

    String userCreatorUUID;
    UserEnhancedWithExternalData userEnhancedCreator;

    User userCreator;

    Activity usingActivity;

    Document usingDocument;


    Workflow usingWorkflow;

    @BeforeEach
    public void setup() {
        userCreator = setUser();

        val createDocumentResponse = new CreateDocumentResponse();
        createDocumentResponse.setId(UUID.randomUUID().toString());
        BDDMockito.given(documentClient.createDocument(any())).willReturn(createDocumentResponse);

        usingActivity = ActivityFixture.generateActivity(userCreator);
        usingActivity.addParticipant(userCreator);
        setWorkflow();
        mockActivityQueries(usingActivity);
        mockDocumentQueries(Set.of(userCreator));
    }

    private User setUser() {
        userEnhancedCreator = UserFixture.generateUserEnhancedWithExternalDataDto();
        userCreatorUUID = userEnhancedCreator.uuid;
        val userCreator = userEnhancedCreator.getUserEntity();
        mockEnhancedUser(userEnhancedCreator);

        PanacheMock.mock(User.class);
        BDDMockito.given(User.findUserByExternalId(userCreatorUUID)).willReturn(Optional.of(userCreator));
        return userCreator;
    }

    private void mockActivityQueries(Activity activity) {
        PanacheMock.mock(Activity.class);
        given(Activity.findByIdOptional(activity.getUuid())).willReturn(Optional.of(activity));
        given(Activity.findById(activity.getUuid())).willReturn(activity);
    }

    private void mockDocumentQueries(Set<User> editors) {
        PanacheMock.mock(Document.class);
        usingDocument = DocumentFixture.createDocument(editors);

        given(Document.findByExternalId(usingDocument.getExternalId())).willReturn(Optional.of(usingDocument));
    }


    private void setWorkflow() {
        PanacheMock.mock(Workflow.class);
        List<Step> steps = List.of(new UnorderedCircleOfWriters());
        val stage = WorkflowFixture.generateStage(ActivityStage.DURING, steps);
        usingWorkflow = WorkflowFixture.generateWorkflow(List.of(stage));

        given(Workflow.findByName(usingWorkflow.getName())).willReturn(Optional.of(usingWorkflow));
    }

    private void mockEnhancedUser(UserEnhancedWithExternalData user) {
        BDDMockito.given(userService.getCompleteUserData(anyString())).willReturn(user);
    }

    @Test
    @DisplayName("[createActivity] Should create an activity (smoke test)")
    public void testActivityCreation() {
        testingThis.createActivity(userCreatorUUID, usingWorkflow.getName());

        Assertions.assertNotNull(usingActivity.getWorkflow());
        Assertions.assertTrue(usingActivity.getGroupActivities().isEmpty());
        then(session).should().persist(any(Activity.class));
        then(userService).should().getCompleteUserData(userCreatorUUID);
    }

    //    Activity creation user validations
    @Test
    @DisplayName("[createActivity] Should throw when creator user is not active and try create activity")
    public void testNotActiveUserTryingToCreateActivity() {
        userEnhancedCreator.isActive = false;

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> testingThis.createActivity(userCreatorUUID, usingWorkflow.getName())).getMessage();

        then(userService).should().getCompleteUserData(userCreatorUUID);
        val expectedException = MessageFormat.format("The user {0} must be active to create an activity", userEnhancedCreator.uuid);
        Assertions.assertEquals(expectedException, exceptionMessage);
    }

    @Test
    @DisplayName("[createActivity] Should throw when user does not have creator role and try create activity")
    public void testNotInRoleUserTryingToCreateActivity() {
        userEnhancedCreator.role.remove(UserRoles.CREATOR);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> testingThis.createActivity(userCreatorUUID, usingWorkflow.getName())).getMessage();

        then(userService).should().getCompleteUserData(userCreatorUUID);
        val expectedException = MessageFormat.format("The user {0} must have role {1} to create an activity", userEnhancedCreator.uuid, UserRoles.CREATOR);
        Assertions.assertEquals(expectedException, exceptionMessage);
    }

    @Test
    @DisplayName("[createActivity] Activity must throw an exception on creation when a workflow is not found")
    public void testActivityWorkflowValidation() {
        val invalidWorkflowName = Faker.instance().aviation().aircraft();
        val exceptionMessage = Assertions.assertThrows(NotFoundException.class, () -> testingThis.createActivity(userCreatorUUID, invalidWorkflowName)).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Workflow with name {0} not found", invalidWorkflowName);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }


    @Test
    @DisplayName("[addUserInActivity] It must let add users to participate")
    public void testAddUserIntoActivity() {
        val activityUuid = usingActivity.getUuid();
        usingActivity.getUserList().remove(userEnhancedCreator);
        userEnhancedCreator.userEntity.setActivity(null);

        testingThis.addUserInActivity(activityUuid, userEnhancedCreator.uuid);
        Activity activity = Activity.findById(activityUuid);

        Assertions.assertEquals(1, activity.getUserList().size());
        Assertions.assertEquals(activity, userCreator.activity);
        Assertions.assertTrue(activity.getUserList().contains(userCreator));
        Assertions.assertEquals(UserStatus.DISCONNECTED, userCreator.status);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if activity exists")
    public void testAddUserValidateActivityExists() {
        val invalidActivityUUID = UUID.randomUUID();
        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> testingThis.addUserInActivity(invalidActivityUUID, userEnhancedCreator.uuid)).getMessage();
        val expectedExceptionMessage = MessageFormat.format("Activity with UUID {0} not found", invalidActivityUUID);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if user is in another activity before try add")
    public void testAddUserAlreadyInAnotherActivity() {
        val newActivity = new Activity();
        newActivity.setUuid(UUID.randomUUID());

        userCreator.activity = newActivity;

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> testingThis.addUserInActivity(usingActivity.getUuid(), userEnhancedCreator.uuid)).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity", userEnhancedCreator.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if USER is active before try add")
    public void testAddUserNotActive() {
        userEnhancedCreator.isActive = false;
        userCreator.setActivity(null);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(usingActivity.getUuid(), userEnhancedCreator.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is not ACTIVE", userEnhancedCreator.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must not change user status if it's with inactive activity")
    public void testSetUserAsAvailableBeforeAddIntoActivity() {

        val anotherActivity = new Activity();
        anotherActivity.setUuid(UUID.randomUUID());
        anotherActivity.isActive = false;

        val userEntity = userCreator;
        userEntity.setActivity(anotherActivity);
        userEntity.status = UserStatus.CONNECTED;
        userEntity.persist();

        val activity = testingThis.addUserInActivity(usingActivity.getUuid(), userEnhancedCreator.uuid);

        Assertions.assertEquals(1, activity.getUserList().size());
        Assertions.assertEquals(activity, userEntity.activity);
        Assertions.assertTrue(activity.getUserList().contains(userEntity));
        Assertions.assertEquals(UserStatus.CONNECTED, userEntity.status);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must format the exception message when validation catch something on add user ")
    public void testAddUserExceptionFormatWithMultipleErrors() {
        val activityUuid = usingActivity.getUuid();
        val activityMock = new Activity();
        activityMock.uuid = UUID.randomUUID();

        userEnhancedCreator
                .getUserEntity()
                .setActivity(activityMock);
        userEnhancedCreator.status = UserStatus.CONNECTED;
        userEnhancedCreator.isActive = false;

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userEnhancedCreator.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity and it is not ACTIVE", userEnhancedCreator.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must validate if it is active before add user")
    public void testAddUserValidateActivityIsActive() {
        val activityUuid = usingActivity.getUuid();
        usingActivity.isActive = false;

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userEnhancedCreator.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Activity {0} must be active to add user {1}", activityUuid, userEnhancedCreator.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must validate if it has not started before add user")
    public void testAddUserValidateActivityHasNotStarted() {
        val activityUuid = usingActivity.getUuid();

        usingActivity.setActualStage(ActivityStage.DURING);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userEnhancedCreator.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Cannot add user {0} to Activity {1} because it has already start", userEnhancedCreator.uuid, activityUuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[startActivity] Should start activity normally")
    public void testActivityStarting() {
        val activityUuid = usingActivity.getUuid();
        val user = userCreator;

        user.status = UserStatus.CONNECTED;
        usingActivity.addParticipant(user);

        testingThis.startActivity(activityUuid);

        Assertions.assertEquals(ActivityStage.DURING, usingActivity.getActualStage());
        then(workflowManageService).should().apply(usingActivity, usingActivity.getCreator(), null);
    }

    @Test
    @DisplayName("[startActivity] Should start activity normally")
    public void testActivityStartingWithGroupRegistered() {
        usingActivity.addGroup(new GroupActivity());
        val user = userCreator;

        user.status = UserStatus.CONNECTED;
        usingActivity.addParticipant(user);

        testingThis.startActivity(usingActivity.getUuid());

        then(groupService).should(never()).createGroup(any(), anySet());
    }

    @Test
    @DisplayName("[startActivity] Should not start inactive activity")
    public void testValidationOfInactiveActivityStarting() {
        val activityUuid = usingActivity.getUuid();
        usingActivity.setIsActive(false);

        val exceptionMessage = Assertions.assertThrows(InvalidActivityActionException.class, () -> {
            testingThis.startActivity(activityUuid);
        }).getMessage();

        val expectedMessage = MessageFormat.format("Activity {0} must be active", usingActivity.uuid);

        Assertions.assertEquals(expectedMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(usingActivity, usingActivity.getCreator(), null);
    }

    @Test
    @DisplayName("[startActivity] Should not start when has no connected user")
    public void testValidationOfNotConnectedActivityStarting() {
        val activityUuid = usingActivity.getUuid();
        userCreator.setStatus(UserStatus.CONNECTED);

        val user = UserFixture.generateUser();
        usingActivity.addParticipant(user);

        val exceptionMessage = Assertions.assertThrows(InvalidActivityActionException.class, () -> {
            testingThis.startActivity(activityUuid);
        }).getMessage();

        val expectedMessage = MessageFormat.format("Activity {0} has the following users not connected: {1}", usingActivity.uuid, List.of(user.getExternalId()));

        Assertions.assertEquals(expectedMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(any(Activity.class), any(User.class), eq(null));
    }

    @Test
    @DisplayName("[startActivity] Should not start when has no users in activity")
    public void testValidationOfEmptyActivity() {
        val activityUuid = usingActivity.getUuid();
        usingActivity.setUserList(new HashSet<>());

        val exceptionMessage = Assertions.assertThrows(InvalidActivityActionException.class, () -> {
            testingThis.startActivity(activityUuid);
        }).getMessage();

        val expectedMessage = MessageFormat.format("Activity {0} has no participants to start", activityUuid);

        Assertions.assertEquals(expectedMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(any(Activity.class), any(User.class), any());
    }

    @Test
    @DisplayName("[startActivity] Should throw when don't find activity")
    public void testValidationOfNotFoundActivityBeforeStartActivity() {
        val activityUuid = UUID.randomUUID();
        val exceptionMessage = Assertions.assertThrows(NotFoundException.class, () -> {
            testingThis.startActivity(activityUuid);
        }).getMessage();

        val expectedMessage = MessageFormat.format("Activity {0} not found", activityUuid);

        Assertions.assertEquals(expectedMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(any(Activity.class), any(User.class), any());
    }

    @Test
    @DisplayName("[startActivity] Should create group if not exists")
    public void testGroupCreationIfNotExistsWhenStartActivity() {
        val activity = ActivityFixture.generateActivity(userCreator);
        val user = userCreator;

        user.status = UserStatus.CONNECTED;
        activity.addParticipant(user);

        PanacheMock.mock(Activity.class);
        BDDMockito.given(Activity.findByIdOptional(any())).willReturn(Optional.of(activity));
        BDDMockito.given(Activity.findById(any())).willReturn(activity);

        testingThis.startActivity(activity.getUuid());
        Assertions.assertFalse(activity.getGroupActivities().isEmpty());
        then(groupService).should().createGroup(eq(activity), anySet());
        then(workflowManageService).should().apply(activity, activity.getCreator(), null);
    }

    @Test
    @DisplayName("[execute] Should execute workflow")
    public void testActivityExecution() throws IOException {
        val content = Faker.instance().howIMetYourMother().catchPhrase();
        val activity = testingThis.execute(generateActivityExecution(content));

        Assertions.assertNotNull(activity);
        then(workflowManageService).should().apply(activity, userCreator, usingDocument);
        then(documentUpdateProducer).should().sendMessage(any(DocumentUpdateDto.class));
    }

    @Test
    @DisplayName("[execute] Should validate not found activity")
    @SneakyThrows
    public void testNotFoundActivityUUID() {
        val randomActivityId = UUID.randomUUID();
        val activityExecutionDto = generateActivityExecution("");
        activityExecutionDto.setActivityUUID(randomActivityId);

        val exceptionMessage = Assertions.assertThrows(NotFoundException.class,
                () -> testingThis.execute(activityExecutionDto)).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Activity {0} not found", randomActivityId);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(any(), any(), any());
        then(documentUpdateProducer).should(never()).sendMessage(any());
    }

    @Test
    @DisplayName("[execute] Should validate not found document")
    @SneakyThrows
    public void testNotFoundDocumentUUID() {
        val randomDocumentId = UUID.randomUUID().toString();
        val activityExecutionDto = generateActivityExecution("");
        activityExecutionDto.setDocumentExternalId(randomDocumentId);

        val exceptionMessage = Assertions.assertThrows(NotFoundException.class,
                () -> testingThis.execute(activityExecutionDto)).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Document {0} not found", randomDocumentId);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(any(), any(), any());
        then(documentUpdateProducer).should(never()).sendMessage(any());
    }

    @Test
    @DisplayName("[execute] Should validate not found user")
    @SneakyThrows
    public void testNotFoundUserUUID() {
        val randomUserId = UUID.randomUUID().toString();
        val activityExecutionDto = generateActivityExecution("");
        activityExecutionDto.setUserExternalId(randomUserId);

        val exceptionMessage = Assertions.assertThrows(NotFoundException.class,
                () -> testingThis.execute(activityExecutionDto)).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} not found", randomUserId);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(any(), any(), any());
        then(documentUpdateProducer).should(never()).sendMessage(any());
    }

    @Test
    @DisplayName("[execute] Should validate if activity is active")
    @SneakyThrows
    public void testExecuteInactiveActivity() {
        usingActivity.setIsActive(false);

        val exceptionMessage = Assertions.assertThrows(InvalidActivityActionException.class,
                () -> testingThis.execute(generateActivityExecution(""))).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Activity {0} must be active", usingActivity.getUuid());
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(any(), any(), any());
        then(documentUpdateProducer).should(never()).sendMessage(any());
    }

    @Test
    @DisplayName("[execute] Should validate if participant is in activity")
    @SneakyThrows
    public void testExecuteParticipantNotInActivity() {
        usingActivity.getUserList().remove(userCreator);

        val exceptionMessage = Assertions.assertThrows(InvalidActivityActionException.class,
                () -> testingThis.execute(generateActivityExecution(""))).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not in activity {1} ", userCreator.getExternalId(), usingActivity.getUuid());
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
        then(workflowManageService).should(never()).apply(any(), any(), any());
        then(documentUpdateProducer).should(never()).sendMessage(any());
    }

    @Test
    @DisplayName("[execute] Should throw message when broker cant send message")
    @SneakyThrows
    public void testBrokerCantSendMessage() {
        willThrow(new IOException("Error")).given(documentUpdateProducer).sendMessage(any());

        val exceptionMessage = Assertions.assertThrows(RuntimeException.class,
                () -> testingThis.execute(generateActivityExecution(""))).getMessage();

        val expectedExceptionMessage = "Error when trying to send update of document to document queue";
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
        then(workflowManageService).should().apply(usingActivity, userCreator, usingDocument);
        then(documentUpdateProducer).should().sendMessage(any());
    }

    private ActivityExecutionDto generateActivityExecution(String content) {
        return new ActivityExecutionDto(
                usingActivity.getUuid(),
                usingDocument.getExternalId(),
                userCreator.getExternalId(),
                content);
    }

    @Test
    @DisplayName("[removeUserFromActivity] Not implemented yet")
    public void testNotImplementedRemoveUserFromActivity() {
        val activity = ActivityFixture.generateActivity(userCreator);
        Assertions.assertThrows(RuntimeException.class, () -> testingThis.removeUserFromActivity(activity.uuid, userCreatorUUID));
    }

    @Test
    @DisplayName("[disconnectUserFromActivity] Not implemented yet")
    public void testNotImplementedDisconnectUserFromActivity() {
        val activity = ActivityFixture.generateActivity(userCreator);
        Assertions.assertThrows(RuntimeException.class, () -> testingThis.disconnectUserFromActivity(activity.uuid, userCreatorUUID));
    }

    @Test
    @DisplayName("[endActivity] - Not implemented yed")
    public void testEndActivityNotImplementedYes() {
        Assertions.assertThrows(NotImplementedYetException.class, () -> testingThis.endActivity(UUID.randomUUID()));
    }
}
