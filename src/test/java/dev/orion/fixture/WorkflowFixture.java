package dev.orion.fixture;

import dev.orion.commom.enums.ActivityStages;
import dev.orion.commom.enums.CircularStepFlowDirectionTypes;
import dev.orion.entity.Stage;
import dev.orion.entity.Step;
import dev.orion.entity.Workflow;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.entity.step_type.ReverseSnowball;
import lombok.val;
import net.datafaker.Faker;

import java.util.List;

public class WorkflowFixture {
    public static Stage generateStage(ActivityStages activityStages, List<Step> steps) {
        val stage = new Stage();
        stage.setStage(activityStages);
        steps.forEach(step -> {
            stage.addStep(step);
        });

        return stage;
    }

    public static Workflow generateWorkflow(List<Stage> stages) {
        val workflow = new Workflow();
        workflow.setName(Faker.instance().rickAndMorty().character());
        workflow.setDescription(Faker.instance().science().element());

        stages.forEach(stage -> {
            workflow.addStepStage(stage);
        });

        return workflow;
    }
}
