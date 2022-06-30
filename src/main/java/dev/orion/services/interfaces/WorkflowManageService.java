package dev.orion.services.interfaces;

import dev.orion.entity.*;

import java.util.Set;

// Control workflow of activity
public interface WorkflowManageService {
    public void apply(Activity activity, User performer, Document document);
    public Workflow createOrUpdateWorkflow(Set<Stage> stages, String name, String description);

    public boolean isFinished(Activity activity);
}
