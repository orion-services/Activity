package dev.orion.entity;

import dev.orion.commom.enums.ActivityStages;
import dev.orion.entity.step_type.CircularStep;
import dev.orion.util.workflow_yaml.CircularStepFlowDirectionTypes;
import io.quarkus.test.junit.QuarkusTest;
import net.datafaker.Faker;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.transaction.Transactional;

@QuarkusTest
@Transactional
public class WorkflowTest {
    private Workflow testingThis;
    private final String WORKFLOW_NAME = Faker.instance().funnyName().name();

    @Test
    public void shouldCreateWorkflowWithAllSteps() {
        final Integer EXPECTED_STEP_STAGES_QTD = 3;
        final Integer EXPECTED_STEPS_QTD = 1;
        StepStage stepStage = generateStepStage(ActivityStages.PRE);
        generateWorkflow(stepStage);
        
        testingThis.addStepStage(generateStepStage(ActivityStages.DURING));
        testingThis.addStepStage(generateStepStage(ActivityStages.AFTER));

        Assertions.assertEquals(testingThis.getStepStages().size(), EXPECTED_STEP_STAGES_QTD);
        Assertions.assertEquals(testingThis.getStepStages().get(ActivityStages.PRE).getSteps().size(), EXPECTED_STEPS_QTD);
        Assertions.assertEquals(testingThis.getStepStages().get(ActivityStages.DURING).getSteps().size(), EXPECTED_STEPS_QTD);
        Assertions.assertEquals(testingThis.getStepStages().get(ActivityStages.AFTER).getSteps().size(), EXPECTED_STEPS_QTD);
    }

    private void generateWorkflow(StepStage stepStage) {
        testingThis = new Workflow();
        testingThis.addStepStage(stepStage);
        testingThis.setName(WORKFLOW_NAME);
        testingThis.persist();
    }

    private StepStage generateStepStage(ActivityStages activityStages) {
        StepStage stepStage = new StepStage();
        stepStage.setStage(activityStages);

        CircularStep circularStep = new CircularStep();
        circularStep.setFlowDirection(CircularStepFlowDirectionTypes.FROM_BEGIN_TO_END);
        circularStep.setName("CIRCLE STEP");

        stepStage.addStep(circularStep);

        return  stepStage;
    }
}
