package dev.orion.services;

import dev.orion.entity.Activity;
import dev.orion.entity.Stage;
import dev.orion.entity.User;
import dev.orion.services.interfaces.WorkflowManageService;
import dev.orion.workflow.CircleStep;
import dev.orion.workflow.StepExecutor;
import lombok.val;

import java.util.HashMap;
import java.util.Map;

public class WorkflowManageServiceImpl implements WorkflowManageService {
    private final Map<String, StepExecutor> stepExecutorsMap = new HashMap<>();

    public WorkflowManageServiceImpl() {
        setupExecutorsMap();
    }

    private void setupExecutorsMap() {
        val circleStep = new CircleStep();
        stepExecutorsMap.put(circleStep.getStepRepresentation(), circleStep);
    }

    @Override
    public void apply(Activity activity, User performer) {
        val actualStage = getActualStage(activity);
        actualStage
                .getSteps()
                .forEach(step -> {
                    stepExecutorsMap.get(step.getType()).execute(activity, performer);
                });
    }

    private Stage getActualStage(Activity activity) {
        return activity.workflow
                .getStages()
                .stream()
                .filter(stage -> stage.getStage().equals(activity.activityStage))
                .findFirst()
                .orElseThrow();
    }
}
