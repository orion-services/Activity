package dev.orion.entity;

import dev.orion.commom.constant.ActivityStages;
import dev.orion.commom.constant.CircularStepFlowDirectionTypes;
import dev.orion.entity.step_type.CircleOfWriters;
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
        Stage stage = generateStepStage(ActivityStages.PRE);
        generateWorkflow(stage);

        testingThis.addStepStage(generateStepStage(ActivityStages.DURING));
        testingThis.addStepStage(generateStepStage(ActivityStages.AFTER));

        Assertions.assertEquals(testingThis.getStages().size(), EXPECTED_STEP_STAGES_QTD);
        Assertions.assertEquals(testingThis.getStages().stream().filter(stage1 -> stage1.getStage() == ActivityStages.PRE).findFirst().orElseThrow().getSteps().size(), EXPECTED_STEPS_QTD);
        Assertions.assertEquals(testingThis.getStages().stream().filter(stage1 -> stage1.getStage() == ActivityStages.DURING).findFirst().orElseThrow().getSteps().size(), EXPECTED_STEPS_QTD);
        Assertions.assertEquals(testingThis.getStages().stream().filter(stage1 -> stage1.getStage() == ActivityStages.AFTER).findFirst().orElseThrow().getSteps().size(), EXPECTED_STEPS_QTD);
    }

    private void generateWorkflow(Stage stage) {
        testingThis = new Workflow();
        testingThis.addStepStage(stage);
        testingThis.setName(WORKFLOW_NAME);
        testingThis.persist();
    }

    private Stage generateStepStage(ActivityStages activityStages) {
        Stage stage = new Stage();
        stage.setStage(activityStages);

        CircleOfWriters circleOfWriters = new CircleOfWriters();
        circleOfWriters.setFlowDirection(CircularStepFlowDirectionTypes.FROM_BEGIN_TO_END);

        stage.addStep(circleOfWriters);

        return stage;
    }
}
