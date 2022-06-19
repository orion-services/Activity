package dev.orion.services;

import dev.orion.commom.constant.ActivityStages;
import dev.orion.commom.constant.UserRoles;
import dev.orion.commom.constant.UserStatus;
import dev.orion.commom.exception.InvalidActivityActionException;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Step;
import dev.orion.entity.User;
import dev.orion.entity.Workflow;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.fixture.UserFixture;
import dev.orion.fixture.WorkflowFixture;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.UserService;
import dev.orion.services.interfaces.WorkflowManageService;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import io.quarkus.test.junit.mockito.InjectSpy;
import lombok.val;
import net.datafaker.Faker;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.NotFoundException;
import java.text.MessageFormat;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

@QuarkusTest
@Transactional
public class ActivityServiceTest {
    @Inject
    ActivityService testingThis;

    @InjectMock
    UserService userService;

    String userCreatorUUID = UUID.randomUUID().toString();

    @InjectSpy
    WorkflowManageService workflowManageService;
    Workflow workflow;


    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        mockEnhancedUser(UserFixture.generateUserEnhancedWithExternalDataDto());
        setWorkflow();
    }

    private void setWorkflow() {
        List<Step> circleOfWriters = List.of(new CircleOfWriters());
        val stage = WorkflowFixture.generateStage(ActivityStages.DURING, circleOfWriters);
        val generateWorkflow = WorkflowFixture.generateWorkflow(List.of(stage));
        workflow = workflowManageService.createOrUpdateWorkflow(
                generateWorkflow.getStages(),
                generateWorkflow.getName(),
                generateWorkflow.getDescription());
    }

    private void mockEnhancedUser(UserEnhancedWithExternalData user) {
        BDDMockito
                .given(userService.getCompleteUserData(anyString()))
                .willReturn(user);
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
        val user = UserFixture.generateUserEnhancedWithExternalDataDto();
        user.isActive = false;

        mockEnhancedUser(user);
        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.createActivity(userCreatorUUID, workflow.getName());
        }).getMessage();

        BDDMockito.then(userService).should().getCompleteUserData(userCreatorUUID);
        val expectedException = MessageFormat.format(
                "The user {0} must be active to create an activity", user.uuid);
        Assertions.assertEquals(
                expectedException,
                exceptionMessage);
    }

    @Test
    @DisplayName("[createActivity] Should throw when user does not have creator role and try create activity")
    public void testNotInRoleUserTryingToCreateActivity() {
        var user = UserFixture.generateUserEnhancedWithExternalDataDto();
        user.role.remove(UserRoles.CREATOR);
        mockEnhancedUser(user);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.createActivity(userCreatorUUID, workflow.getName());
        }).getMessage();

        BDDMockito.then(userService).should().getCompleteUserData(userCreatorUUID);
        val expectedException = MessageFormat.format(
                "The user {0} must have role {1} to create an activity", user.uuid, UserRoles.CREATOR);
        Assertions.assertEquals(
                expectedException,
                exceptionMessage);
    }

    @Test
    @DisplayName("[createActivity] Activity must throw an exception on creation when a workflow is not found")
    public void testActivityWorkflowValidation() {
        val invalidWorkflowName =  Faker.instance().aviation().aircraft();
        val exceptionMessage = Assertions.assertThrows(NotFoundException.class, () -> {
            testingThis.createActivity(userCreatorUUID, invalidWorkflowName);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format(
                "Workflow with name {0} not found", invalidWorkflowName);
        Assertions.assertEquals(
                expectedExceptionMessage,
                exceptionMessage);
    }

//    Add user into activity
    @Test
    @DisplayName("[addUserInActivity] It must let add users to participate")
    public void testAddUserIntoActivity() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        mockEnhancedUser(userEnhancedWithExternalData);


        testingThis.addUserInActivity(activityUuid, userEnhancedWithExternalData.uuid);
        Activity activity = Activity.findById(activityUuid);

        Assertions.assertEquals(1, activity.getUserList().size());
        Assertions.assertEquals(activity, userEnhancedWithExternalData.getUserEntity().activity);
        Assertions.assertTrue(activity.getUserList().contains(userEnhancedWithExternalData.getUserEntity()));
        Assertions.assertEquals(UserStatus.DISCONNECTED, userEnhancedWithExternalData.getUserEntity().status);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if activity exists")
    public void testAddUserValidateActivityExists() {
        val user = UserFixture.generateUserEnhancedWithExternalDataDto();
        val invalidActivityUUID = UUID.randomUUID();
        mockEnhancedUser(user);
        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(invalidActivityUUID, user.uuid);
        }).getMessage();
        val expectedExceptionMessage = MessageFormat.format("Activity with UUID {0} not found", invalidActivityUUID);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if user is in another activity before try add")
    public void testAddUserAlreadyInAnotherActivity() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        userEnhancedWithExternalData.getUserEntity().activity = new Activity();
        mockEnhancedUser(userEnhancedWithExternalData);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userEnhancedWithExternalData.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity", userEnhancedWithExternalData.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must validate if USER is active before try add")
    public void testAddUserNotActive() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        userEnhancedWithExternalData.isActive = false;
        mockEnhancedUser(userEnhancedWithExternalData);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userEnhancedWithExternalData.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is not ACTIVE", userEnhancedWithExternalData.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] It must not change user status if it's with inactive activity")
    public void testSetUserAsAvailableBeforeAddIntoActivity() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        mockEnhancedUser(userEnhancedWithExternalData);

        val anotherActivityId = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val anotherActivity = (Activity) Activity.findById(anotherActivityId);
        anotherActivity.isActive = false;

        val userEntity = userEnhancedWithExternalData.getUserEntity();
        userEntity.setActivity(anotherActivity);
        userEntity.status = UserStatus.CONNECTED;
        userEntity.persist();

        val activity = testingThis.addUserInActivity(activityUuid, userEnhancedWithExternalData.uuid);

        Assertions.assertEquals(1, activity.getUserList().size());
        Assertions.assertEquals(activity, userEntity.activity);
        Assertions.assertTrue(activity.getUserList().contains(userEntity));
        Assertions.assertEquals(UserStatus.CONNECTED, userEntity.status);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must format the exception message when validation catch something on add user ")
    public void testAddUserExceptionFormatWithMultipleErrors() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        userEnhancedWithExternalData.getUserEntity().activity = new Activity();
        userEnhancedWithExternalData.status = UserStatus.CONNECTED;
        userEnhancedWithExternalData.isActive = false;

        mockEnhancedUser(userEnhancedWithExternalData);

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userEnhancedWithExternalData.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity and it is not ACTIVE", userEnhancedWithExternalData.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must validate if it is active before add user")
    public void testAddUserValidateActivityIsActive() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        mockEnhancedUser(userEnhancedWithExternalData);

        Activity activity = Activity.findById(activityUuid);
        activity.isActive = false;
        activity.persist();

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userEnhancedWithExternalData.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Activity {0} must be active to add user {1}", activityUuid, userEnhancedWithExternalData.uuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[addUserInActivity] Activity must validate if it has not started before add user")
    public void testAddUserValidateActivityHasNotStarted() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        mockEnhancedUser(userEnhancedWithExternalData);

        Activity activity = Activity.findById(activityUuid);
        activity.setActualStage(ActivityStages.DURING);
        activity.persist();

        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testingThis.addUserInActivity(activityUuid, userEnhancedWithExternalData.uuid);
        }).getMessage();

        val expectedExceptionMessage = MessageFormat.format("Cannot add user {0} to Activity {1} because it has already start", userEnhancedWithExternalData.uuid, activityUuid);
        Assertions.assertEquals(expectedExceptionMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[startActivity] Should start activity normally")
    public void testActivityStarting() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        val activity = (Activity) Activity.findById(activityUuid);
        testingThis.startActivity(activityUuid);

        Assertions.assertEquals(ActivityStages.DURING ,activity.getActualStage());
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
    @DisplayName("[startActivity] Should not start inactive activity")
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
    @DisplayName("[startActivity] Should create group if not exists")
    public void testGroupCreationIfNotExistsWhenStartActivity() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());
        testingThis.startActivity(activityUuid);

        val activity = (Activity) Activity.findById(activityUuid);
        Assertions.assertFalse(activity.getGroupActivities().isEmpty());
        BDDMockito.then(workflowManageService).should().apply(activity, activity.getCreator());
    }
}
