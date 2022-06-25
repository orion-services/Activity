package dev.orion.services;

import dev.orion.client.DocumentClient;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.commom.constant.ActivityStages;
import dev.orion.commom.constant.UserRoles;
import dev.orion.commom.constant.UserStatus;
import dev.orion.commom.exception.InvalidActivityActionException;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Step;
import dev.orion.entity.User;
import dev.orion.entity.Workflow;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.fixture.ActivityFixture;
import dev.orion.fixture.UserFixture;
import dev.orion.fixture.WorkflowFixture;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.GroupService;
import dev.orion.services.interfaces.UserService;
import dev.orion.services.interfaces.WorkflowManageService;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.junit.QuarkusMock;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import io.quarkus.test.junit.mockito.InjectSpy;
import lombok.val;
import net.datafaker.Faker;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;

import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.NotFoundException;
import java.text.MessageFormat;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.*;

@QuarkusTest
@Transactional
public class ActivityServiceTest {
    private final String userCreatorUUID = UUID.randomUUID().toString();
    @Inject
    ActivityService testingThis;
    @InjectMock
    UserService userService;
    @InjectSpy
    GroupService groupService;
    @InjectMock
    @RestClient
    DocumentClient documentClient;
    Session session;
    @InjectSpy
    WorkflowManageService workflowManageService;
    Workflow workflow;
    private UserEnhancedWithExternalData userCreator;

    @BeforeEach
    public void setup() {
        userCreator = UserFixture.generateUserEnhancedWithExternalDataDto();
        userCreator.uuid = userCreatorUUID;
        mockEnhancedUser(userCreator);

        val createDocumentResponse = new CreateDocumentResponse();
        createDocumentResponse.setId(UUID.randomUUID().toString());
        BDDMockito.given(documentClient.createDocument(any())).willReturn(createDocumentResponse);

        setWorkflow();
    }

    private void setWorkflow() {
        List<Step> circleOfWriters = List.of(new CircleOfWriters());
        val stage = WorkflowFixture.generateStage(ActivityStages.DURING, circleOfWriters);
        val generateWorkflow = WorkflowFixture.generateWorkflow(List.of(stage));
        workflow = workflowManageService.createOrUpdateWorkflow(generateWorkflow.getStages(), generateWorkflow.getName(), generateWorkflow.getDescription());
    }

    private void mockEnhancedUser(UserEnhancedWithExternalData user) {
        BDDMockito.given(userService.getCompleteUserData(anyString())).willReturn(user);
    }

    @Test
    @DisplayName("[createActivity] Should create an activity (smoke test)")
    public void testActivityCreation() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());

        Activity activity = Activity.findById(activityUuid);

        Assertions.assertNotNull(activityUuid);
        Assertions.assertNotNull(activity.getWorkflow());
        Assertions.assertTrue(activity.getUserList().isEmpty());
        Assertions.assertTrue(activity.getGroupActivities().isEmpty());
    }

    //    Activity creation user validations
    @Test
    @DisplayName("[createActivity] Should throw when creator user is not active and try create activity")
    public void testNotActiveUserTryingToCreateActivity() {
        userCreator.isActive = false;

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> testingThis.createActivity(userCreatorUUID, workflow.getName())).getMessage();

        BDDMockito.then(userService).should().getCompleteUserData(userCreatorUUID);
        val expectedException = MessageFormat.format("The user {0} must be active to create an activity", userCreator.uuid);
        Assertions.assertEquals(expectedException, exceptionMessage);
    }

    @Test
    @DisplayName("[createActivity] Should throw when user does not have creator role and try create activity")
    public void testNotInRoleUserTryingToCreateActivity() {
        userCreator.role.remove(UserRoles.CREATOR);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> testingThis.createActivity(userCreatorUUID, workflow.getName())).getMessage();

        BDDMockito.then(userService).should().getCompleteUserData(userCreatorUUID);
        val expectedException = MessageFormat.format("The user {0} must have role {1} to create an activity", userCreator.uuid, UserRoles.CREATOR);
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
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());

        testingThis.addUserInActivity(activityUuid, userCreator.uuid);
        Activity activity = Activity.findById(activityUuid);

        Assertions.assertEquals(1, activity.getUserList().size());
        Assertions.assertEquals(activity, userCreator.getUserEntity().activity);
        Assertions.assertTrue(activity.getUserList().contains(userCreator.getUserEntity()));
        Assertions.assertEquals(UserStatus.DISCONNECTED, userCreator.getUserEntity().status);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if activity exists")
    public void testAddUserValidateActivityExists() {
        val invalidActivityUUID = UUID.randomUUID();
        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> testingThis.addUserInActivity(invalidActivityUUID, userCreator.uuid)).getMessage();
        val expectedExceptionMessage = MessageFormat.format("Activity with UUID {0} not found", invalidActivityUUID);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if user is in another activity before try add")
    public void testAddUserAlreadyInAnotherActivity() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        userCreator.getUserEntity().activity = new Activity();

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> testingThis.addUserInActivity(activityUuid, userCreator.uuid)).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity", userCreator.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if USER is active before try add")
    public void testAddUserNotActive() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        userCreator.isActive = false;

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userCreator.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is not ACTIVE", userCreator.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must not change user status if it's with inactive activity")
    public void testSetUserAsAvailableBeforeAddIntoActivity() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());

        val anotherActivityId = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val anotherActivity = (Activity) Activity.findById(anotherActivityId);
        anotherActivity.isActive = false;

        val userEntity = userCreator.getUserEntity();
        userEntity.setActivity(anotherActivity);
        userEntity.status = UserStatus.CONNECTED;
        userEntity.persist();

        val activity = testingThis.addUserInActivity(activityUuid, userCreator.uuid);

        Assertions.assertEquals(1, activity.getUserList().size());
        Assertions.assertEquals(activity, userEntity.activity);
        Assertions.assertTrue(activity.getUserList().contains(userEntity));
        Assertions.assertEquals(UserStatus.CONNECTED, userEntity.status);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must format the exception message when validation catch something on add user ")
    public void testAddUserExceptionFormatWithMultipleErrors() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());

        userCreator.getUserEntity().activity = new Activity();
        userCreator.status = UserStatus.CONNECTED;
        userCreator.isActive = false;

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userCreator.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity and it is not ACTIVE", userCreator.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must validate if it is active before add user")
    public void testAddUserValidateActivityIsActive() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());

        Activity activity = Activity.findById(activityUuid);
        activity.isActive = false;

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userCreator.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Activity {0} must be active to add user {1}", activityUuid, userCreator.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must validate if it has not started before add user")
    public void testAddUserValidateActivityHasNotStarted() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());

        Activity activity = Activity.findById(activityUuid);
        activity.setActualStage(ActivityStages.DURING);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userCreator.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Cannot add user {0} to Activity {1} because it has already start", userCreator.uuid, activityUuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[startActivity] Should start activity normally")
    public void testActivityStarting() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val activity = (Activity) Activity.findById(activityUuid);
        val user = userCreator.getUserEntity();

        user.status = UserStatus.CONNECTED;
        activity.addParticipant(user);

        testingThis.startActivity(activityUuid);

        Assertions.assertEquals(ActivityStages.DURING, activity.getActualStage());
        BDDMockito.then(workflowManageService).should().apply(activity, activity.getCreator());
    }

    @Test
    @DisplayName("[startActivity] Should not start inactive activity")
    public void testValidationOfInactiveActivityStarting() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val activity = (Activity) Activity.findById(activityUuid);
        activity.setIsActive(false);

        val exceptionMessage = Assertions.assertThrows(InvalidActivityActionException.class, () -> {
            testingThis.startActivity(activityUuid);
        }).getMessage();

        val expectedMessage = MessageFormat.format("Activity {0} must be active", activity.uuid);

        Assertions.assertEquals(expectedMessage, exceptionMessage);
        BDDMockito.then(workflowManageService).should(BDDMockito.never()).apply(activity, activity.getCreator());
    }

    @Test
    @DisplayName("[startActivity] Should not start when has no connected user")
    public void testValidationOfNotConnectedActivityStarting() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val activity = (Activity) Activity.findById(activityUuid);

        val user = UserFixture.generateUser();
        activity.addParticipant(user);

        val exceptionMessage = Assertions.assertThrows(InvalidActivityActionException.class, () -> {
            testingThis.startActivity(activityUuid);
        }).getMessage();

        val expectedMessage = MessageFormat.format("Activity {0} has the following users not connected: {1}", activity.uuid, List.of(user.getExternalId()));

        Assertions.assertEquals(expectedMessage, exceptionMessage);
        BDDMockito.then(workflowManageService).should(BDDMockito.never()).apply(any(Activity.class), any(User.class));
    }

    @Test
    @DisplayName("[startActivity] Should not start when has no users in activity")
    public void testValidationOfEmptyActivity() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val exceptionMessage = Assertions.assertThrows(InvalidActivityActionException.class, () -> {
            testingThis.startActivity(activityUuid);
        }).getMessage();

        val expectedMessage = MessageFormat.format("Activity {0} has no participants to start", activityUuid);

        Assertions.assertEquals(expectedMessage, exceptionMessage);
        BDDMockito.then(workflowManageService).should(BDDMockito.never()).apply(any(Activity.class), any(User.class));
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
        BDDMockito.then(workflowManageService).should(BDDMockito.never()).apply(any(Activity.class), any(User.class));
    }

    @Test
    @DisplayName("[startActivity] Should create group if not exists")
    public void testGroupCreationIfNotExistsWhenStartActivity() {
        mockHibernateSession();
        val activity = ActivityFixture.generateActivity(userCreator.getUserEntity());
        val user = userCreator.getUserEntity();

        user.status = UserStatus.CONNECTED;
        activity.addParticipant(user);

        PanacheMock.mock(Activity.class);
        BDDMockito.given(Activity.findByIdOptional(any())).willReturn(Optional.of(activity));
        BDDMockito.given(Activity.findById(any())).willReturn(activity);

        testingThis.startActivity(activity.getUuid());
        Assertions.assertFalse(activity.getGroupActivities().isEmpty());
        BDDMockito.then(groupService).should().createGroup(eq(activity), anySet());
        BDDMockito.then(workflowManageService).should().apply(activity, activity.getCreator());
    }

    private void mockHibernateSession() {
        session = Mockito.mock(Session.class);
        QuarkusMock.installMockForType(session, Session.class);
        BDDMockito.doNothing().when(session).persist(any());
    }
}
