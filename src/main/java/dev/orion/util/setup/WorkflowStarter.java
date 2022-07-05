package dev.orion.util.setup;

import dev.orion.commom.constant.ActivityStage;
import dev.orion.entity.Stage;
import dev.orion.entity.step_type.SendEmailStep;
import dev.orion.entity.step_type.UnorderedCircleOfWriters;
import dev.orion.services.interfaces.WorkflowManageService;
import dev.orion.util.AggregateException;
import io.quarkus.arc.log.LoggerName;
import io.quarkus.runtime.StartupEvent;
import lombok.val;
import net.datafaker.Faker;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.event.Observes;
import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

@ApplicationScoped
public class WorkflowStarter {
    @LoggerName("WorkflowStarter")
    Logger LOGGER;

    @Inject
    WorkflowManageService manageService;

    public static final String GENERIC_WORKFLOW_NAME =  "simple_workflow";
    public static final String COMPLETE_WORKFLOW_NAME =  "complete_workflow";

    void onStart(@Observes StartupEvent ev) {
        LOGGER.info("Creating or updating workflows");
        try {
            generateCompleteWorkflow(); // <<<<<------ USING THIS!
            generateSimpleWorkflow();
        } catch (AggregateException aggregateException) {
            aggregateException.getExceptions().forEach(runtimeException -> {
                LOGGER.errorv("Error while generating workflows {0}", runtimeException.getMessage());
            });

            throw new RuntimeException("There was an error while creating workflows");
        }
    }



    private void generateSimpleWorkflow() {
        val stages = generateMockStages();
        val name = GENERIC_WORKFLOW_NAME;
        val description = Faker.instance().yoda().quote();

        val workflow = manageService.createOrUpdateWorkflow(stages, name, description);

        LOGGER.info(MessageFormat.format("Created workflow with name {0}", workflow.getName()));
    }

    private void generateCompleteWorkflow() {
        val stages = generateMockStages();
        val sendEmailStage = new Stage();
        sendEmailStage.setActivityStage(ActivityStage.PRE);
        val sendEmailStep = new SendEmailStep();
        sendEmailStep.addMessage(ActivityStage.PRE, "The activity is starting!");
        sendEmailStep.addMessage(ActivityStage.POS, "The activity has ended! :D");
        sendEmailStage.addStep(sendEmailStep);

//        Validation [DEMONSTRATION] step auto-validation
//        val duringStage = stages.stream().filter(stage -> stage.getActivityStage().equals(ActivityStage.DURING)).findFirst().orElseThrow();
//        duringStage.addStep(sendEmailStep);
//        ---------
        stages.add(sendEmailStage);

        val name = COMPLETE_WORKFLOW_NAME;
        val description = Faker.instance().yoda().quote();

        val workflow = manageService.createOrUpdateWorkflow(stages, name, description);

        LOGGER.info(MessageFormat.format("Created workflow with name {0}", workflow.getName()));
    }

    private Set<Stage> generateMockStages() {
        val stage = new Stage();
        stage.setActivityStage(ActivityStage.DURING);
        stage.addStep(new UnorderedCircleOfWriters());

        val stages = new HashSet<Stage>();
        stages.add(stage);

        return stages;
    }
}
