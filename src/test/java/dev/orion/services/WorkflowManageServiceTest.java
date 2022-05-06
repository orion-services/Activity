package dev.orion.services;

import dev.orion.commom.enums.ActivityStages;
import dev.orion.commom.enums.CircularStepFlowDirectionTypes;
import dev.orion.commom.exceptions.IncompleteWorkflowException;
import dev.orion.entity.*;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.entity.step_type.ReverseSnowball;
import dev.orion.fixture.UserFixture;
import dev.orion.workflow.CircleStepExecutor;
import dev.orion.workflow.ReverseSnowBallStepExecutor;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import net.datafaker.Faker;
import org.junit.jupiter.api.*;
import org.mockito.BDDMockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.*;

import static org.mockito.Mockito.*;

@QuarkusTest
@Transactional
public class WorkflowManageServiceTest {
    @Inject
    WorkflowManageServiceImpl workflowManageService;

    @InjectMock
    CircleStepExecutor circleStepExecutor;

    @InjectMock
    ReverseSnowBallStepExecutor reverseSnowBallStepExecutor;


    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        doNothing().when(circleStepExecutor).execute(any(), any());
        when(circleStepExecutor.getStepRepresentation()).thenCallRealMethod();
        when(reverseSnowBallStepExecutor.getStepRepresentation()).thenCallRealMethod();
    }

    @Test
    @DisplayName("Should call the right step by activity the phase")
    public void testShouldCallTheRightStepByActivityPhase() {
        User user = UserFixture.generateUser();
        user.id = null;
        user.persistAndFlush();
        Activity activity = new Activity();
        activity.createdBy = user;

        //  Test incoming from database
        activity.workflow = generateWorkflow();

        activity.persistAndFlush();

        //  Test incoming from database
        Activity persistedActivity = Activity.findById(activity.uuid);

        workflowManageService.apply(persistedActivity, user);
        BDDMockito.then(circleStepExecutor).should().execute(any(), any());
        BDDMockito.then(reverseSnowBallStepExecutor).should(times(0)).execute(any(), any());

        persistedActivity.actualStage = ActivityStages.DURING;
        workflowManageService.apply(persistedActivity, user);
        BDDMockito.then(circleStepExecutor).should(times(1)).execute(any(), any());
        BDDMockito.then(reverseSnowBallStepExecutor).should().execute(any(), any());
    }

    @Test
    @DisplayName("Should throw error when workflow has no steps")
    public void testShouldThrowErrorWhenWorkflowHasNoStep() {
        Workflow workflow = new Workflow();
        Stage emptyStage = new Stage();
        emptyStage.setStage(ActivityStages.PRE);

        workflow.setName(Faker.instance().rickAndMorty().character());
        workflow.setDescription(Faker.instance().science().element());
        workflow.addStepStage(emptyStage);

        User user = UserFixture.generateUser();
        Activity activity = new Activity();
        activity.createdBy = user;
        activity.workflow = workflow;

        Assertions.assertThrows(IncompleteWorkflowException.class, () -> workflowManageService.apply(activity, user));
    }

    private Workflow generateWorkflow() {
        val workflow = new Workflow();
        workflow.setName(Faker.instance().rickAndMorty().character());
        workflow.setDescription(Faker.instance().science().element());
        workflow.addStepStage(generateStage(ActivityStages.PRE));
        workflow.addStepStage(generateStage(ActivityStages.DURING));
        workflow.persistAndFlush();

        return workflow;
    }

    private Stage generateStage(ActivityStages activityStages) {
        val stage = new Stage();
        stage.setStage(activityStages);
        if (activityStages.equals(ActivityStages.PRE)) {
            stage.addStep(new CircleOfWriters(CircularStepFlowDirectionTypes.FROM_BEGIN_TO_END));
            return stage;
        }
        stage.addStep(new ReverseSnowball());
        return stage;
    }
}
