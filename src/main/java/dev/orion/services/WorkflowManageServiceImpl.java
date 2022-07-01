package dev.orion.services;

import dev.orion.commom.constant.ActivityStage;
import dev.orion.commom.exception.InvalidActivityActionException;
import dev.orion.commom.exception.InvalidWorkflowConfiguration;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import dev.orion.services.interfaces.WorkflowManageService;
import dev.orion.util.AggregateException;
import dev.orion.workflowExecutor.StepExecutor;
import dev.orion.workflowExecutor.impl.CircleStepExecutor;
import dev.orion.workflowExecutor.impl.ReverseSnowBallStepExecutor;
import dev.orion.workflowExecutor.impl.SendEmailStepExecutor;
import dev.orion.workflowExecutor.impl.UnorderedCircleOfWritersStepExecutor;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;

import javax.annotation.PostConstruct;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Collectors;

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

    @Inject
    SendEmailStepExecutor sendEmailStepExecutor;

    @LoggerName("WorkflowManageServiceImpl")
    Logger logger;

    @PostConstruct
    void setupExecutorsMap() {
        stepExecutorsMap.put(reverseSnowBallStepExecutor.getStepRepresentation(), reverseSnowBallStepExecutor);
        stepExecutorsMap.put(unorderedCircleOfWritersStepExecutor.getStepRepresentation(), unorderedCircleOfWritersStepExecutor);
        stepExecutorsMap.put(sendEmailStepExecutor.getStepRepresentation(), sendEmailStepExecutor);
    }

    @Override
    public void apply(Activity activity, User performer, Document document) throws InvalidWorkflowConfiguration {
        val actualStageOpt = extractActualStage(activity);
        if (actualStageOpt.isEmpty()) {
            return;
        }

        val actualStage = actualStageOpt.get();
        if (actualStage.getSteps().isEmpty()) {
            String errorMessage = MessageFormat.format("There is no steps on workflow \"{0}\" in stage {1}", activity.workflow.getName(), actualStageOpt.get().getActivityStage());
            logger.error(errorMessage);
            throw new InvalidWorkflowConfiguration(errorMessage);
        }

        var executionQueue = createExecutionQueue(actualStage.getSteps(), activity, performer, document);

        while (!executionQueue.isEmpty()) {
            executionQueue.poll().run();
        }
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

    @Override
    public Workflow createOrUpdateWorkflow(Set<Stage> stages, String name, String description) {
        validateIfWorkflowHasDuringStages(stages);
        validateIfAllStagesAreWellConfigured(stages);

        val workflow = (Workflow) Workflow.findByName(name).orElse(new Workflow());
        workflow.setName(name);
        workflow.setDescription(description);
        workflow.setStages(stages);

        workflow.persist();

        return workflow;
    }

    private void validateIfWorkflowHasDuringStages(Set<Stage> stages) {
        if (stages.stream().noneMatch(stage -> stage.getActivityStage().equals(ActivityStage.DURING))) {
            throw new InvalidWorkflowConfiguration("Cannot create workflow without have a DURING phase stage");
        }
    }

    private void validateIfAllStagesAreWellConfigured(Set<Stage> stages) {
        stages.forEach(this::validateStage);
    }

    private void validateStage(Stage stage) {
        List<RuntimeException> exceptionList = new ArrayList<>();

        val stepsWithExecutors = stage.getSteps().stream().filter(this::hasExecutorForStep).collect(Collectors.toList());
        stepsWithExecutors.forEach(step -> {
            try {
                val stepExecutor = stepExecutorsMap.get(step.getType());
                stepExecutor.validateConfig(stage);
            } catch (InvalidWorkflowConfiguration invalidWorkflowConfiguration) {
                exceptionList.add(invalidWorkflowConfiguration);
            }
        });

        if (!exceptionList.isEmpty()) {
            logger.warnv("Workflow is not valid because of the following errors: {0}", exceptionList);
            throw new AggregateException(exceptionList);
        }
    }

    @Override
    public boolean isFinished(Activity activity) {
        val stage = extractActualStage(activity).orElseThrow();
        if (Boolean.FALSE == activity.getActualStage().equals(ActivityStage.DURING)) {
            val exceptionMessage = MessageFormat.format("To check if workflow is finished activity should be in stage {0}", ActivityStage.DURING);
            logger.error(exceptionMessage);
            throw new InvalidActivityActionException(exceptionMessage);
        }
        val steps = stage.getSteps();
        val stepStream = steps.stream().filter(this::hasExecutorForStep);

        return stepStream.allMatch(step -> stepExecutorsMap.get(step.getType()).isFinished(activity, step));
    }




    private Optional<Stage> extractActualStage(Activity activity) {
        val actualStage = activity.workflow.getStages().stream().filter(stage -> stage.getActivityStage().equals(activity.actualStage)).findFirst();

        if (actualStage.isEmpty()) {
            logger.info(MessageFormat.format("There is no {0} stage on activity {1}", activity.actualStage, activity.uuid));
        }
        return actualStage;
    }


    private boolean hasExecutorForStep(Step step) {
        return stepExecutorsMap.containsKey(step.getType());
    }
}
