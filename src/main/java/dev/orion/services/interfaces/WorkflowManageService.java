package dev.orion.services.interfaces;

import dev.orion.entity.Activity;
import dev.orion.entity.Stage;
import dev.orion.entity.User;
import dev.orion.entity.Workflow;

import java.util.Set;

// Control workflow of activity
public interface WorkflowManageService {
    public void apply(Activity activity, User performer);
    public Workflow createOrUpdateWorkflow(Set<Stage> stages, String name, String description);
}
