package dev.orion.services;

import dev.orion.commom.exceptions.IncompleteWorkflowException;
import dev.orion.commom.exceptions.NotValidActionException;
import dev.orion.entity.Activity;
import dev.orion.entity.Stage;
import dev.orion.entity.Step;
import dev.orion.entity.User;
import dev.orion.services.interfaces.WorkflowManageService;
import dev.orion.util.AggregateException;
import dev.orion.workflow.CircleStepExecutor;
import dev.orion.workflow.ReverseSnowBallStepExecutor;
import dev.orion.workflow.StepExecutor;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.*;

@ApplicationScoped
public class WorkflowManageServiceImpl implements WorkflowManageService {
    private final Map<String, StepExecutor> stepExecutorsMap = new HashMap<>();

    @Inject
    CircleStepExecutor circleStepExecutor;

    @Inject
    ReverseSnowBallStepExecutor reverseSnowBallStepExecutor;

    @LoggerName("WorkflowManageServiceImpl")
    Logger logger;

    private void setupExecutorsMap() {
        stepExecutorsMap.put(circleStepExecutor.getStepRepresentation(), circleStepExecutor);
        stepExecutorsMap.put(reverseSnowBallStepExecutor.getStepRepresentation(), reverseSnowBallStepExecutor);
    }

    @Override
    public void apply(Activity activity, User performer) throws IncompleteWorkflowException {
        setupExecutorsMap();

        val actualStageOpt = extractActualStage(activity);


        val actualStage = actualStageOpt.get();
        if (actualStage.getSteps().isEmpty()) {
            String errorMessage = MessageFormat.format("There is no steps on workflow \"{0}\" in stage {1}", activity.workflow.getName(), actualStageOpt.get().getStage());
            logger.error(errorMessage);
            throw new IncompleteWorkflowException(errorMessage);
        }

        var executionQueue = createExecutionQueue(actualStage.getSteps(), activity, performer);

        while (!executionQueue.isEmpty()) {
            executionQueue.poll().run();
        }
    }

    private Queue<Runnable> createExecutionQueue(List<Step> steps, Activity activity, User performer) {
        Queue<Runnable> executionQueue = new LinkedList<>();
        List<RuntimeException> exceptionList = new ArrayList<>();

        steps.stream().filter(step -> Objects.nonNull(stepExecutorsMap.get(step.getType()))).forEach(step -> {
            val stepExecutor = stepExecutorsMap.get(step.getType());
            try {
                stepExecutor.validate(activity, performer);
                executionQueue.add(() -> stepExecutor.execute(activity, performer));
            } catch (NotValidActionException notValidActionException) {
                logger.warn("Step: '" + notValidActionException.getStepName() + "' validation throw when trying to apply to activity: " + activity.uuid);
                exceptionList.add(notValidActionException);
            }
        });

//        Throw together all exceptions provided by each step.
        if (!exceptionList.isEmpty()) {
            throw new AggregateException(exceptionList);
        }

        return executionQueue;
    }

    private Optional<Stage> extractActualStage(Activity activity) {
        val actualStage = activity.workflow.getStages().stream().filter(stage -> stage.getStage().equals(activity.actualStage)).findFirst();

        if (actualStage.isEmpty()) {
            logger.info(MessageFormat.format("There is no {0} stage on activity {1}", activity.actualStage, activity.uuid));
        }
        return actualStage;
    }
}
