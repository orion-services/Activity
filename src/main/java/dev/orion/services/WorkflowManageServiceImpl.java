package dev.orion.services;

import dev.orion.commom.exceptions.IncompleteWorkflowException;
import dev.orion.entity.Activity;
import dev.orion.entity.Stage;
import dev.orion.entity.User;
import dev.orion.entity.step_type.ReverseSnowball;
import dev.orion.services.interfaces.WorkflowManageService;
import dev.orion.workflow.CircleStep;
import dev.orion.workflow.ReverseSnowBallStep;
import dev.orion.workflow.StepExecutor;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@ApplicationScoped
public class WorkflowManageServiceImpl implements WorkflowManageService {
    private final Map<String, StepExecutor> stepExecutorsMap = new HashMap<>();

    @Inject
    CircleStep circleStep;

    @Inject
    ReverseSnowBallStep reverseSnowBallStep;

    @LoggerName("WorkflowManageServiceImpl")
    Logger logger;

    private void setupExecutorsMap() {
        stepExecutorsMap.put(circleStep.getStepRepresentation(), circleStep);
        stepExecutorsMap.put(reverseSnowBallStep.getStepRepresentation(), reverseSnowBallStep);
    }

    @Override
    public void apply(Activity activity, User performer) throws IncompleteWorkflowException {
        setupExecutorsMap();

        val actualStageOpt = extractActualStage(activity);
        if (actualStageOpt.isEmpty()) {
            logger.info(MessageFormat.format("There is no {0} stage on activity {1}", activity.actualStage, activity.uuid));
            return;
        }

        val actualStage = actualStageOpt.get();
        if (actualStage.getSteps().isEmpty()) {
            String errorMessage = MessageFormat.format("There is no steps on workflow \"{0}\" in stage {1}", activity.workflow.getName(), actualStageOpt.get().getStage());
            logger.error(errorMessage);
            throw new IncompleteWorkflowException(errorMessage);
        }

        actualStage
                .getSteps()
                .forEach(step -> {
                    StepExecutor stepExecutor = stepExecutorsMap.get(step.getType());
                    if (stepExecutor != null) {
                        stepExecutor.execute(activity, performer);
                    }
                });
    }

    private Optional<Stage> extractActualStage(Activity activity) {
        return activity.workflow
                .getStages()
                .stream()
                .filter(stage -> stage.getStage().equals(activity.actualStage))
                .findFirst();
    }
}
