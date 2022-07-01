package dev.orion.fixture;

import dev.orion.commom.constant.ActivityStage;
import dev.orion.commom.constant.CircularStepFlowDirectionTypes;
import dev.orion.entity.Activity;
import dev.orion.entity.Step;
import dev.orion.entity.User;
import dev.orion.entity.Workflow;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.util.setup.WorkflowStarter;
import lombok.val;

import java.util.List;
import java.util.UUID;

public class ActivityFixture {
    public static Activity generateActivity(User user) {
        val activityUuid = UUID.randomUUID();
        Activity activity = new Activity();
        activity.uuid = activityUuid;
        activity.isActive = true;
        activity.setCreator(user);
        user.setActivity(activity);

        val stepList = List.of(new Step[]{new CircleOfWriters(CircularStepFlowDirectionTypes.FROM_BEGIN_TO_END)});
        val stage = WorkflowFixture.generateStage(ActivityStage.DURING, stepList);
        val workflow = WorkflowFixture.generateWorkflow(List.of(stage));
        workflow.setName(WorkflowStarter.GENERIC_WORKFLOW_NAME);
        activity.setWorkflow(workflow);

        return activity;
    }

    public static Activity generateActivity(User user, Workflow workflow) {
        val activityUuid = UUID.randomUUID();
        Activity activity = new Activity();
        activity.uuid = activityUuid;
        activity.isActive = true;
        activity.setCreator(user);
        activity.setWorkflow(workflow);

        return activity;
    }
}
