package dev.orion.services;

import dev.orion.commom.constant.ActivityStage;
import dev.orion.commom.exception.InvalidActivityActionException;
import dev.orion.commom.exception.InvalidWorkflowConfiguration;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import dev.orion.entity.step_type.SendEmailStep;
import dev.orion.entity.step_type.UnorderedCircleOfWriters;
import dev.orion.fixture.UserFixture;
import dev.orion.fixture.WorkflowFixture;
import dev.orion.util.AggregateException;
import dev.orion.workflowExecutor.impl.SendEmailStepExecutor;
import dev.orion.workflowExecutor.impl.UnorderedCircleOfWritersStepExecutor;
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
    UnorderedCircleOfWritersStepExecutor unorderedCircleOfWritersStepExecutor;

    @InjectMock
    SendEmailStepExecutor sendEmailStepExecutor;

    @InjectMock
    Session session;

    private UnorderedCircleOfWriters unorderedCircleOfWriters = new UnorderedCircleOfWriters();

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        given(sendEmailStepExecutor.getStepRepresentation()).willCallRealMethod();
        given(unorderedCircleOfWritersStepExecutor.getStepRepresentation()).willCallRealMethod();
        given(sendEmailStepExecutor.getStepRepresentation()).willCallRealMethod();
        PanacheMock.mock(Workflow.class);
    }

    @Test
    @DisplayName("[apply] Should call the right step by activity phase")
    public void testShouldCallTheRightStepByActivityPhase() {
        val sendEmail = new SendEmailStep();
        val allowedStages = sendEmail.getAllowedStages();
        Assertions.assertEquals(2, allowedStages.size());

        User user = UserFixture.generateUser();

        Activity activity = new Activity();
        activity.creator = user;
        activity.workflow = generateWorkflow();

        testThis.apply(activity, user, new Document());
        BDDMockito.then(sendEmailStepExecutor).should().execute(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWritersStepExecutor).should(never()).execute(any(), any(), any());

        activity.actualStage = ActivityStage.DURING;
        testThis.apply(activity, user, new Document());
        BDDMockito.then(sendEmailStepExecutor).should().execute(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWritersStepExecutor).should().execute(any(), any(), any());
    }

    @Test
    @DisplayName("[apply] Should call the validation of each step")
    public void testShouldCallValidationForEachStep() {
        User user = UserFixture.generateUser();

        Activity activity = new Activity();
        activity.creator = user;
        activity.workflow = generateWorkflow();

        testThis.apply(activity, user, new Document());
        activity.actualStage = ActivityStage.DURING;
        testThis.apply(activity, user, new Document());
        BDDMockito.then(sendEmailStepExecutor).should().validate(any(), any(), any(SendEmailStep.class));
        BDDMockito.then(unorderedCircleOfWritersStepExecutor).should().validate(any(), any(), any(UnorderedCircleOfWriters.class));
    }

    @Test
    @DisplayName("[apply] Should throw error when a stage do not validate")
    public void testStageThrowValidation() {
        BDDMockito
                .willThrow(new NotValidActionException("unorderedCircleOfWriterStepExecutor", "error"))
                .given(unorderedCircleOfWritersStepExecutor)
                .validate(any(), any(), any());
        willThrow(new NotValidActionException("sendEmailStepExecutor", "error"))
                .given(sendEmailStepExecutor)
                .validate(any(), any(), any());

        User user = UserFixture.generateUser();
        user.persist();

        Activity activity = new Activity();
        activity.creator = user;
        activity.workflow = generateWorkflow();
        activity.workflow.getStages().forEach(stage -> {
            if (stage.getActivityStage().equals(ActivityStage.PRE)) {
                stage.addStep(new UnorderedCircleOfWriters());
            }
        });

        activity.persist();

        val aggregateException = Assertions.assertThrows(AggregateException.class, () -> testThis.apply(activity, user, null));

        Assertions.assertEquals(2, aggregateException.getExceptions().size());
        BDDMockito.then(sendEmailStepExecutor).should(never()).execute(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWritersStepExecutor).should(never()).execute(any(), any(), any());
        BDDMockito.then(sendEmailStepExecutor).should().validate(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWritersStepExecutor).should().validate(any(), any(), any());
    }

    @Test
    @DisplayName("[apply] Should do nothing when there's no step in stage")
    public void testNoActualStageThrowValidation() {
        Activity activity = new Activity();
        activity.creator = UserFixture.generateUser();
        activity.workflow = generateWorkflow();

        activity.setActualStage(ActivityStage.POS);

        testThis.apply(activity, activity.getCreator(), new Document());
        BDDMockito.then(sendEmailStepExecutor).should(never()).execute(any(), any(), any());
        BDDMockito.then(unorderedCircleOfWritersStepExecutor).should(never()).execute(any(), any(), any());
    }

    @Test
    @DisplayName("[apply] Should throw error when workflow has no stages")
    public void testShouldThrowErrorWhenWorkflowHasNoStep() {
        Workflow workflow = new Workflow();
        Stage emptyStage = new Stage();
        emptyStage.setActivityStage(ActivityStage.PRE);

        workflow.setName(Faker.instance().rickAndMorty().character());
        workflow.setDescription(Faker.instance().science().element());
        workflow.addStepStage(emptyStage);

        User user = UserFixture.generateUser();
        Activity activity = new Activity();
        activity.creator = user;
        activity.workflow = workflow;

        Assertions.assertThrows(InvalidWorkflowConfiguration.class, () -> testThis.apply(activity, user, new Document()));
        BDDMockito.then(unorderedCircleOfWritersStepExecutor).should(never()).execute(any(), any(), any());
        BDDMockito.then(sendEmailStepExecutor).should(never()).execute(any(), any(), any());
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
        val stepList = List.of(new Step[]{new SendEmailStep()});
        val stages = Set.of(WorkflowFixture.generateStage(ActivityStage.DURING, stepList));

        val workflow = testThis.createOrUpdateWorkflow(stages, name, description);

        Assertions.assertNotNull(workflow);
        Assertions.assertNotNull(workflow.id);
        Assertions.assertEquals(name, workflow.getName());
        Assertions.assertEquals(description, workflow.getDescription());
        Assertions.assertFalse(workflow.getStages().isEmpty());
        then(session).should().persist(workflow);
    }

    @Test
    @DisplayName("[createOrUpdateWorkflow] Workflow creation must have at least on DURING stage")
    public void testCreationWithoutDuringPhase() {
        val name = Faker.instance().rickAndMorty().character();
        val description = Faker.instance().science().element();
        val stepList = List.of(new Step[]{new SendEmailStep()});
        val stages = Set.of(WorkflowFixture.generateStage(ActivityStage.PRE, stepList));

        Assertions.assertThrows(InvalidWorkflowConfiguration.class, () -> testThis.createOrUpdateWorkflow(stages, name, description));
    }

    @Test
    @DisplayName("[createOrUpdateWorkflow] Should update an workflow")
    public void testWorkflowUpdate() {
        val description = Faker.instance().science().element();
        val stepList = List.of(new Step[]{new SendEmailStep()});
        val stages = Arrays.asList(WorkflowFixture.generateStage(ActivityStage.DURING, stepList));
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
    @DisplayName("[createOrUpdateWorkflow] Workflow creation must have validate the stage")
    public void testCreationInvalidationOfExecutor() {
        val name = Faker.instance().rickAndMorty().character();
        val description = Faker.instance().science().element();
        val stepList = Arrays.asList(unorderedCircleOfWriters, new SendEmailStep());

        val stages = Set.of(WorkflowFixture.generateStage(ActivityStage.DURING, stepList));

        willThrow(new InvalidWorkflowConfiguration(Faker.instance().witcher().quote())).given(unorderedCircleOfWritersStepExecutor).validateConfig(any(Stage.class));
        willThrow(new InvalidWorkflowConfiguration(Faker.instance().yoda().quote())).given(sendEmailStepExecutor).validateConfig(any(Stage.class));

        val exceptionMessage = Assertions.assertThrows(AggregateException.class, () -> testThis.createOrUpdateWorkflow(stages, name, description)).getMessage();
    }

    @Test
    @DisplayName("[isFinished] - Should return true when all stages are finished")
    public void testWorkflowFinished() {
        Workflow workflow = generateWorkflow();
        val stage = workflow.getStages().stream().filter(testStage -> testStage.getActivityStage().equals(ActivityStage.DURING)).findFirst().get();
        stage.addStep(new SendEmailStep());

        Activity activity = new Activity();
        activity.setActualStage(ActivityStage.DURING);
        activity.workflow = workflow;
        given(unorderedCircleOfWritersStepExecutor.isFinished(activity, unorderedCircleOfWriters)).willReturn(true);
        given(sendEmailStepExecutor.isFinished(eq(activity), any(SendEmailStep.class))).willReturn(true);


        val finished = testThis.isFinished(activity);

        Assertions.assertTrue(finished);
        then(unorderedCircleOfWritersStepExecutor).should().isFinished(activity, unorderedCircleOfWriters);
        then(sendEmailStepExecutor).should().isFinished(eq(activity), any(SendEmailStep.class));
    }

    @Test
    @DisplayName("[isFinished] - Should return true when all stages are finished")
    public void testWorkflowFinishedIfSomeStageIsNotFinished() {
        Workflow workflow = generateWorkflow();
        val stage = workflow.getStages().stream().filter(testStage -> testStage.getActivityStage().equals(ActivityStage.DURING)).findFirst().get();
        stage.addStep(new SendEmailStep());

        Activity activity = new Activity();
        activity.setActualStage(ActivityStage.DURING);
        activity.workflow = workflow;
        given(unorderedCircleOfWritersStepExecutor.isFinished(activity, unorderedCircleOfWriters)).willReturn(true);
        given(sendEmailStepExecutor.isFinished(eq(activity), any(SendEmailStep.class))).willReturn(false);


        val finished = testThis.isFinished(activity);

        Assertions.assertFalse(finished);
        then(unorderedCircleOfWritersStepExecutor).should().isFinished(activity, unorderedCircleOfWriters);
        then(sendEmailStepExecutor).should().isFinished(eq(activity), any(SendEmailStep.class));
    }

    @Test
    @DisplayName("[isFinished] - Should validate if stage of activity is not PRE")
    public void testWorkflowStageValidationInIsFinished() {
        Workflow workflow = generateWorkflow();
        Activity activity = new Activity();
        activity.workflow = workflow;

        Assertions.assertThrows(InvalidActivityActionException.class, () -> testThis.isFinished(activity));

        then(unorderedCircleOfWritersStepExecutor).should(never()).isFinished(any(Activity.class), any());
    }

    @Test
    @DisplayName("[isFinished] - Should return true if activity stage is POS")
    public void testIsFinishedTrueIfActivityStageIsPOS() {
        Workflow workflow = generateWorkflow();
        Activity activity = new Activity();
        activity.setActualStage(ActivityStage.POS);
        activity.workflow = workflow;

        val finished = testThis.isFinished(activity);

        then(unorderedCircleOfWritersStepExecutor).should(never()).isFinished(any(Activity.class), any());
        Assertions.assertTrue(finished);
    }

    private Workflow generateWorkflow() {
        val workflow = new Workflow();
        workflow.setName(Faker.instance().rickAndMorty().character());
        workflow.setDescription(Faker.instance().science().element());
        workflow.addStepStage(generateStage(ActivityStage.PRE));
        workflow.addStepStage(generateStage(ActivityStage.DURING));
        workflow.persist();

        return workflow;
    }

    private Stage generateStage(ActivityStage activityStage) {
        val stage = new Stage();
        stage.setActivityStage(activityStage);
        if (activityStage.equals(ActivityStage.PRE)) {
            stage.addStep(new SendEmailStep());
            return stage;
        }
        stage.addStep(unorderedCircleOfWriters);
        return stage;
    }
}
