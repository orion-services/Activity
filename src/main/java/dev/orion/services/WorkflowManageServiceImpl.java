package dev.orion.services;

import dev.orion.commom.constant.ActivityStages;
import dev.orion.commom.exception.IncompleteWorkflowException;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import dev.orion.services.interfaces.WorkflowManageService;
import dev.orion.util.AggregateException;
import dev.orion.workflowExecutor.CircleStepExecutor;
import dev.orion.workflowExecutor.ReverseSnowBallStepExecutor;
import dev.orion.workflowExecutor.StepExecutor;
import dev.orion.workflowExecutor.UnorderedCircleOfWritersStepExecutor;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;

import javax.annotation.PostConstruct;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;
import java.util.*;

@ApplicationScoped
@Transactional
public class WorkflowManageServiceImpl implements WorkflowManageService {
    private final Map<String, StepExecutor> stepExecutorsMap = new HashMap<>();

    @Inject
    CircleStepExecutor circleStepExecutor;

    @Inject
    ReverseSnowBallStepExecutor reverseSnowBallStepExecutor;

    @Inject
    UnorderedCircleOfWritersStepExecutor unorderedCircleOfWritersStepExecutor;

    @LoggerName("WorkflowManageServiceImpl")
    Logger logger;

    @PostConstruct
    private void setupExecutorsMap() {
        stepExecutorsMap.put(circleStepExecutor.getStepRepresentation(), circleStepExecutor);
        stepExecutorsMap.put(reverseSnowBallStepExecutor.getStepRepresentation(), reverseSnowBallStepExecutor);
        stepExecutorsMap.put(unorderedCircleOfWritersStepExecutor.getStepRepresentation(), unorderedCircleOfWritersStepExecutor);
    }

    @Override
    public void apply(Activity activity, User performer, Document document) throws IncompleteWorkflowException {
        val actualStageOpt = extractActualStage(activity);
        if (actualStageOpt.isEmpty()) {
            return;
        }

        val actualStage = actualStageOpt.get();
        if (actualStage.getSteps().isEmpty()) {
            String errorMessage = MessageFormat.format("There is no steps on workflow \"{0}\" in stage {1}", activity.workflow.getName(), actualStageOpt.get().getStage());
            logger.error(errorMessage);
            throw new IncompleteWorkflowException(errorMessage);
        }

        var executionQueue = createExecutionQueue(actualStage.getSteps(), activity, performer, document);

        while (!executionQueue.isEmpty()) {
            executionQueue.poll().run();
        }
    }

    @Override
    public Workflow createOrUpdateWorkflow(Set<Stage> stages, String name, String description) {
        if (stages.stream().noneMatch(stage -> stage.getStage().equals(ActivityStages.DURING))) {
            throw new IncompleteWorkflowException("Cannot create workflow without have a DURING phase stage");
        }

        val workflow = (Workflow) Workflow.findByName(name).orElse(new Workflow());
        workflow.setName(name);
        workflow.setDescription(description);
        workflow.setStages(stages);

        workflow.persist();

        return workflow;
    }

    @Override
    public boolean isFinished(Activity activity) {
        return false;
    }


    private Queue<Runnable> createExecutionQueue(List<Step> steps, Activity activity, User performer, Document document) {
        Queue<Runnable> executionQueue = new LinkedList<>();
        List<RuntimeException> exceptionList = new ArrayList<>();

        steps.stream().filter(this::hasExecutorForStep).forEach(step -> {
            val stepExecutor = stepExecutorsMap.get(step.getType());
            try {
                stepExecutor.validate(document, performer, step);
                executionQueue.add(() -> stepExecutor.execute(document, performer, step));
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

    private boolean hasExecutorForStep(Step step) {
        return stepExecutorsMap.containsKey(step.getType());
    }
}
