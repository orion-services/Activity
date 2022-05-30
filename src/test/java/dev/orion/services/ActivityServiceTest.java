package dev.orion.services;

import dev.orion.commom.enums.ActivityStages;
import dev.orion.entity.Activity;
import dev.orion.entity.Step;
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
import lombok.val;
import org.apache.commons.lang3.NotImplementedException;
import org.junit.jupiter.api.*;
import org.mockito.ArgumentMatchers;
import org.mockito.BDDMockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.List;
import java.util.UUID;

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
                .given(userService.getCompleteUserData(ArgumentMatchers.anyString()))
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
        Assertions.assertNotNull(workflow);
        Assertions.assertTrue(activity.getUserList().isEmpty());
        Assertions.assertFalse(activity.getGroupActivities().isEmpty());
    }

    @Test
    @DisplayName("Should call API to check if the user can create activities")
    public void testShouldCreateAnActivityWithUser() {
        throw new NotImplementedException();
    }

    @Test
    @DisplayName("Activity must have a group")
    public void testActivityHasGroup() {
        throw new NotImplementedException();
    }

    @Test
    @DisplayName("Activity must have a workflow")
    public void testActivityHasWorkflow() {
        throw new NotImplementedException();
    }

    @Test
    @DisplayName("Activity must throw an exception on creation when a workflow is not found")
    public void testActivityWorkflowValidation() {
        throw new NotImplementedException();
    }
}
