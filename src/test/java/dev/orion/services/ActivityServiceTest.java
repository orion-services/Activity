package dev.orion.services;

import dev.orion.commom.constant.ActivityStages;
import dev.orion.commom.constant.UserRoles;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Step;
import dev.orion.entity.Workflow;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.fixture.UserFixture;
import dev.orion.fixture.WorkflowFixture;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.UserService;
import dev.orion.services.interfaces.WorkflowManageService;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import org.apache.commons.lang3.NotImplementedException;
import org.junit.jupiter.api.*;
import org.mockito.BDDMockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;
import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.anyString;

@QuarkusTest
@Transactional
public class ActivityServiceTest {
    @Inject
    ActivityService testingThis;

    @InjectMock
    UserService userService;

    String userCreatorUUID = UUID.randomUUID().toString();

    @Inject
    WorkflowManageService workflowManageService;
    Workflow workflow;


    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        BDDMockito
                .given(userService.getCompleteUserData(anyString()))
                .willReturn(UserFixture.generateUserEnhancedWithExternalDataDto());
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

    @Test
    @DisplayName("Should create an activity (smoke test)")
    public void testActivityCreation() {
        val activityUuid = testingThis.createActivity(userCreatorUUID, workflow.getName());

        Activity activity = Activity.findById(activityUuid);

        Assertions.assertNotNull(activityUuid);
        Assertions.assertNotNull(activity.getWorkflow());
        Assertions.assertTrue(activity.getUserList().isEmpty());
        Assertions.assertFalse(activity.getGroupActivities().isEmpty());
    }

//    Activity creation user validations
    @Test
    @DisplayName("Should throw when creator user is not active and try create activity")
    public void testNotActiveUserTryingToCreateActivity() {
        val user = UserFixture.generateUserEnhancedWithExternalDataDto();
        user.isActive = false;

        BDDMockito
                .given(userService.getCompleteUserData(anyString()))
                .willReturn(user);
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
    @DisplayName("Should throw when creator user is not active and try create activity")
    public void testNotInRoleUserTryingToCreateActivity() {
        val user = UserFixture.generateUserEnhancedWithExternalDataDto();
        user.isActive = false;
        user.role.add(UserRoles.CREATOR);

        BDDMockito
                .given(userService.getCompleteUserData(anyString()))
                .willReturn(user);
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
    @DisplayName("Activity must throw an exception on creation when a workflow is not found")
    public void testActivityWorkflowValidation() {
        throw new NotImplementedException();
    }
}
