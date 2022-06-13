package dev.orion.util.setup;

import dev.orion.commom.constant.ActivityStages;
import dev.orion.entity.Stage;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.services.interfaces.WorkflowManageService;
import io.quarkus.arc.log.LoggerName;
import io.quarkus.runtime.StartupEvent;
import lombok.val;
import net.datafaker.Faker;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.event.Observes;
import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.Set;

@ApplicationScoped
public class WorkflowStarter {
    @LoggerName("WorkflowStarter")
    Logger LOGGER;

    @Inject
    WorkflowManageService manageService;

    void onStart(@Observes StartupEvent ev) {
        LOGGER.info("Creating or updating workflows");

        val stages = generateMockStages();
        val name = "Workflow_test";
        val description = Faker.instance().yoda().quote();

        val workflow = manageService.createOrUpdateWorkflow(stages, name, description);

        LOGGER.info(MessageFormat.format("Created workflow with name {0}", workflow.getName()));
    }

    private Set<Stage> generateMockStages() {
        val stage = new Stage();
        stage.setStage(ActivityStages.DURING);
        stage.addStep(new CircleOfWriters());

        return Set.of(stage);
    }
}
