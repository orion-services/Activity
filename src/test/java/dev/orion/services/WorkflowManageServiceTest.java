package dev.orion.services;

import dev.orion.commom.constant.ActivityStages;
import dev.orion.commom.constant.CircularStepFlowDirectionTypes;
import dev.orion.commom.exception.IncompleteWorkflowException;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.entity.step_type.UnorderedCircleOfWriters;
import dev.orion.fixture.UserFixture;
import dev.orion.fixture.WorkflowFixture;
import dev.orion.util.AggregateException;
import dev.orion.workflowExecutor.CircleStepExecutor;
import dev.orion.workflowExecutor.UnorderedCircleOfWriterStepExecutor;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import net.datafaker.Faker;
import org.hibernate.Session;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.BDDMockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.*;

import static org.mockito.BDDMockito.*;

@QuarkusTest
@Transactional
public class WorkflowManageServiceTest {
    
    @Inject
    WorkflowManageServiceImpl testThis;

    @InjectMock
    CircleStepExecutor circleStepExecutor;

    @InjectMock
    UnorderedCircleOfWriterStepExecutor unorderedCircleOfWriterStepExecutor;

    @InjectMock
    Session session;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        when(circleStepExecutor.getStepRepresentation()).thenCallRealMethod();
        when(unorderedCircleOfWriterStepExecutor.getStepRepresentation()).thenCallRealMethod();
        PanacheMock.mock(Workflow.class);
    }

    @Test
    @DisplayName("[apply] Should call the right step by activity phase")
    public void testShouldCallTheRightStepByActivityPhase() {
        User user = UserFixture.generateUser();

        Activity activity = new Activity();
        activity.creator = user;
        activity.workflow = generateWorkflow();

        testThis.apply(activity, user, new Document());
        BDDMockito.then(circleStepExecutor).should().execute(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWriterStepExecutor).should(times(0)).execute(any(), any(), any());

        activity.actualStage = ActivityStages.DURING;
        testThis.apply(activity, user, new Document());
        BDDMockito.then(circleStepExecutor).should(times(1)).execute(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWriterStepExecutor).should().execute(any(), any(), any());
    }

    @Test
    @DisplayName("[apply] Should call the validation of each step")
    public void testShouldCallValidationForEachStep() {
        User user = UserFixture.generateUser();

        Activity activity = new Activity();
        activity.creator = user;
        activity.workflow = generateWorkflow();


        testThis.apply(activity, user, new Document());
        activity.actualStage = ActivityStages.DURING;
        testThis.apply(activity, user, new Document());
        BDDMockito.then(circleStepExecutor).should(atLeastOnce()).validate(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWriterStepExecutor).should(atLeastOnce()).validate(any(), any(), any());
    }

    @Test
    @DisplayName("[apply] Should throw error when a stage do not validate")
    public void testStageThrowValidation() {
        BDDMockito
                .willThrow(new NotValidActionException("unorderedCircleOfWriterStepExecutor", "error"))
                .given(unorderedCircleOfWriterStepExecutor)
                .validate(any(), any(), any());
        BDDMockito
                .willThrow(new NotValidActionException("circleStepExecutor", "error"))
                .given(circleStepExecutor)
                .validate(any(), any(), any());

        User user = UserFixture.generateUser();
        user.persist();

        Activity activity = new Activity();
        activity.creator = user;
        activity.workflow = generateWorkflow();
        activity.workflow.getStages().forEach(stage -> {
            if (stage.getStage().equals(ActivityStages.PRE)) {
                stage.addStep(new UnorderedCircleOfWriters());
            }
        });

        activity.persist();

        val aggregateException = Assertions.assertThrows(AggregateException.class, () -> testThis.apply(activity, user, null));

        Assertions.assertEquals(2, aggregateException.getExceptions().size());
        BDDMockito.then(circleStepExecutor).should(never()).execute(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWriterStepExecutor).should(never()).execute(any(), any(), any());
        BDDMockito.then(circleStepExecutor).should().validate(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWriterStepExecutor).should().validate(any(), any(), any());
    }

    @Test
    @DisplayName("[apply] Should do nothing when there's no step in stage")
    public void testNoActualStageThrowValidation() {
        Activity activity = new Activity();
        activity.creator = UserFixture.generateUser();
        activity.workflow = generateWorkflow();

        activity.setActualStage(ActivityStages.POS);

        testThis.apply(activity, activity.getCreator(), new Document());
        BDDMockito.then(circleStepExecutor).should(never()).execute(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWriterStepExecutor).should(never()).execute(any(), any(), any());
    }

    @Test
    @DisplayName("[apply] Should throw error when workflow has no stages")
    public void testShouldThrowErrorWhenWorkflowHasNoStep() {
        Workflow workflow = new Workflow();
        Stage emptyStage = new Stage();
        emptyStage.setStage(ActivityStages.PRE);

        workflow.setName(Faker.instance().rickAndMorty().character());
        workflow.setDescription(Faker.instance().science().element());
        workflow.addStepStage(emptyStage);

        User user = UserFixture.generateUser();
        Activity activity = new Activity();
        activity.creator = user;
        activity.workflow = workflow;

        Assertions.assertThrows(IncompleteWorkflowException.class, () -> testThis.apply(activity, user, new Document()));
        BDDMockito.then(unorderedCircleOfWriterStepExecutor).should(never()).execute(any(), any(), any());
        BDDMockito.then(circleStepExecutor).should(never()).execute(any(), any(), any());
    }

//   Workflow creation with createWorkflow
    @Test
    @DisplayName("[createOrUpdateWorkflow] Should create an workflow")
    public void testWorkflowCreation() {
        given(Workflow.findByName(anyString())).willReturn(Optional.empty());
        willAnswer(invocation -> {
            val workflow = invocation.getArgument(0, Workflow.class);
            workflow.id = 2L;
            return null;
        }).given(session).persist(any(Workflow.class));
        val name = Faker.instance().rickAndMorty().character();
        val description = Faker.instance().science().element();
        val stepList = List.of(new Step[]{new CircleOfWriters(CircularStepFlowDirectionTypes.FROM_BEGIN_TO_END)});
        val stages = Set.of(WorkflowFixture.generateStage(ActivityStages.DURING, stepList));

        val workflow = testThis.createOrUpdateWorkflow(stages, name, description);

        Assertions.assertNotNull(workflow);
        Assertions.assertNotNull(workflow.id);
        Assertions.assertEquals(name, workflow.getName());
        Assertions.assertEquals(description, workflow.getDescription());
        Assertions.assertFalse(workflow.getStages().isEmpty());
        then(session).should().persist(workflow);
    }

    @Test
    @DisplayName("[createOrUpdateWorkflow] Workflow creation must have at least stage for during phase")
    public void testCreationWithoutDuringPhase() {
        val name = Faker.instance().rickAndMorty().character();
        val description = Faker.instance().science().element();
        val stepList = List.of(new Step[]{new CircleOfWriters(CircularStepFlowDirectionTypes.FROM_BEGIN_TO_END)});
        val stages = Set.of(WorkflowFixture.generateStage(ActivityStages.PRE, stepList));

        Assertions.assertThrows(IncompleteWorkflowException.class, () -> testThis.createOrUpdateWorkflow(stages, name, description));
    }

    @Test
    @DisplayName("[createOrUpdateWorkflow] Should update an workflow")
    public void testWorkflowUpdate() {
        val description = Faker.instance().science().element();
        val stepList = List.of(new Step[]{new CircleOfWriters(CircularStepFlowDirectionTypes.FROM_BEGIN_TO_END)});
        val stages = Arrays.asList(WorkflowFixture.generateStage(ActivityStages.DURING, stepList));
        val workflow = WorkflowFixture.generateWorkflow(stages);
        workflow.id = 1L;

        given(Workflow.findByName(anyString())).willReturn(Optional.of(workflow));
        val expectedId = workflow.id;
        val workflowName = workflow.getName();

        val newWorkflow = testThis.createOrUpdateWorkflow(new HashSet<>(stages), workflowName, Faker.instance().science().scientist());

        newWorkflow.setName(Faker.instance().rickAndMorty().character());
        Assertions.assertNotNull(newWorkflow);
        Assertions.assertEquals(expectedId, newWorkflow.id);
        Assertions.assertNotEquals(newWorkflow.getDescription(), description);
    }

    @Test
    @DisplayName("[isFinished] - ")
    public void test() {

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
        stage.addStep(new UnorderedCircleOfWriters());
        return stage;
    }
}
