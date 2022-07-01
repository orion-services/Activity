package dev.orion.entity;

import dev.orion.commom.constant.ActivityStage;
import dev.orion.entity.step_type.SendEmailStep;
import io.quarkus.test.junit.QuarkusTest;
import net.datafaker.Faker;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import javax.transaction.Transactional;

@QuarkusTest
@Transactional
public class WorkflowTest {
    private final String WORKFLOW_NAME = Faker.instance().funnyName().name();
    private Workflow testingThis;

    @Test
    public void shouldCreateWorkflowWithAllSteps() {
        final Integer EXPECTED_STEP_STAGES_QTD = 3;
        final Integer EXPECTED_STEPS_QTD = 1;
        Stage stage = generateStepStage(ActivityStage.PRE);
        generateWorkflow(stage);

        testingThis.addStepStage(generateStepStage(ActivityStage.DURING));
        testingThis.addStepStage(generateStepStage(ActivityStage.POS));

        Assertions.assertEquals(testingThis.getStages().size(), EXPECTED_STEP_STAGES_QTD);
        Assertions.assertEquals(testingThis.getStages().stream().filter(stage1 -> stage1.getActivityStage() == ActivityStage.PRE).findFirst().orElseThrow().getSteps().size(), EXPECTED_STEPS_QTD);
        Assertions.assertEquals(testingThis.getStages().stream().filter(stage1 -> stage1.getActivityStage() == ActivityStage.DURING).findFirst().orElseThrow().getSteps().size(), EXPECTED_STEPS_QTD);
        Assertions.assertEquals(testingThis.getStages().stream().filter(stage1 -> stage1.getActivityStage() == ActivityStage.POS).findFirst().orElseThrow().getSteps().size(), EXPECTED_STEPS_QTD);
    }

    private void generateWorkflow(Stage stage) {
        testingThis = new Workflow();
        testingThis.addStepStage(stage);
        testingThis.setName(WORKFLOW_NAME);
        testingThis.persist();
    }

    private Stage generateStepStage(ActivityStage activityStage) {
        Stage stage = new Stage();
        stage.setActivityStage(activityStage);

        stage.addStep(new SendEmailStep());

        return stage;
    }
}
