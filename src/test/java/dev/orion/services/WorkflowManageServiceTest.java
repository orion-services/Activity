package dev.orion.services;

import dev.orion.commom.enums.ActivityStages;
import dev.orion.commom.enums.CircularStepFlowDirectionTypes;
import dev.orion.commom.exceptions.IncompleteWorkflowException;
import dev.orion.commom.exceptions.NotValidActionException;
import dev.orion.entity.Activity;
import dev.orion.entity.Stage;
import dev.orion.entity.User;
import dev.orion.entity.Workflow;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.entity.step_type.ReverseSnowball;
import dev.orion.fixture.UserFixture;
import dev.orion.util.AggregateException;
import dev.orion.workflow.CircleStepExecutor;
import dev.orion.workflow.ReverseSnowBallStepExecutor;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import net.datafaker.Faker;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.BDDMockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;

import static org.mockito.Mockito.*;

@QuarkusTest
@Transactional
public class WorkflowManageServiceTest {
    
    @Inject
    WorkflowManageServiceImpl testThis;

    @InjectMock
    CircleStepExecutor circleStepExecutor;

    @InjectMock
    ReverseSnowBallStepExecutor reverseSnowBallStepExecutor;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        when(circleStepExecutor.getStepRepresentation()).thenCallRealMethod();
        when(reverseSnowBallStepExecutor.getStepRepresentation()).thenCallRealMethod();
    }

    @Test
    @DisplayName("Should call the right step by activity phase")
    public void testShouldCallTheRightStepByActivityPhase() {
        User user = UserFixture.generateUser();
        user.id = null;
        user.persist();

        Activity activity = new Activity();
        activity.createdBy = user;
        activity.workflow = generateWorkflow();
        activity.persist();

        //  Test incoming from database
        Activity persistedActivity = Activity.findById(activity.uuid);

        testThis.apply(persistedActivity, user);
        BDDMockito.then(circleStepExecutor).should().execute(any(), any());
        BDDMockito.then(reverseSnowBallStepExecutor).should(times(0)).execute(any(), any());

        persistedActivity.actualStage = ActivityStages.DURING;
        testThis.apply(persistedActivity, user);
        BDDMockito.then(circleStepExecutor).should(times(1)).execute(any(), any());
        BDDMockito.then(reverseSnowBallStepExecutor).should().execute(any(), any());
    }

    @Test
    @DisplayName("Should call the validation of each step")
    public void testShouldCallValidationForEachStep() {
        User user = UserFixture.generateUser();
        user.id = null;
        user.persist();

        Activity activity = new Activity();
        activity.createdBy = user;

        //  Test incoming from database
        activity.workflow = generateWorkflow();

        activity.persist();

        Activity persistedActivity = Activity.findById(activity.uuid);

        testThis.apply(persistedActivity, user);
        persistedActivity.actualStage = ActivityStages.DURING;
        testThis.apply(persistedActivity, user);
        BDDMockito.then(circleStepExecutor).should(atLeastOnce()).validate(any(), any());
        BDDMockito.then(reverseSnowBallStepExecutor).should(atLeastOnce()).validate(any(), any());
    }

    @Test
    @DisplayName("Should throw error when a stage do not validate")
    public void testStageThrowValidation() {
        BDDMockito
                .willThrow(new NotValidActionException("reverseSnowBallStepExecutor", "error"))
                .given(reverseSnowBallStepExecutor)
                .validate(any(), any());
        BDDMockito
                .willThrow(new NotValidActionException("reverseSnowBallStepExecutor", "error"))
                .given(circleStepExecutor)
                .validate(any(), any());

        User user = UserFixture.generateUser();

        Activity activity = new Activity();
        activity.createdBy = user;
        activity.workflow = generateWorkflow();
        activity.workflow.getStages().forEach(stage -> {
            if (stage.getStage().equals(ActivityStages.PRE)) {
                stage.addStep(new ReverseSnowball());
            }
        });

        val aggregateException = Assertions.assertThrows(AggregateException.class, () -> testThis.apply(activity, user));

        Assertions.assertEquals(2, aggregateException.getExceptions().size());
        BDDMockito.then(circleStepExecutor).should(never()).execute(any(), any());
        BDDMockito.then(reverseSnowBallStepExecutor).should(never()).execute(any(), any());
        BDDMockito.then(circleStepExecutor).should().validate(any(), any());
        BDDMockito.then(reverseSnowBallStepExecutor).should().validate(any(), any());
    }

    @Test
    @DisplayName("Should throw error when workflow has no stages")
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

        Assertions.assertThrows(IncompleteWorkflowException.class, () -> testThis.apply(activity, user));
        BDDMockito.then(reverseSnowBallStepExecutor).should(never()).execute(any(), any());
        BDDMockito.then(circleStepExecutor).should(never()).execute(any(), any());
    }

    private Workflow generateWorkflow() {
        val workflow = new Workflow();
        workflow.setName(Faker.instance().rickAndMorty().character());
        workflow.setDescription(Faker.instance().science().element());
        workflow.addStepStage(generateStage(ActivityStages.PRE));
        workflow.addStepStage(generateStage(ActivityStages.DURING));
        workflow.persist();

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
