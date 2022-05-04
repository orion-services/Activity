package dev.orion.workflow;

import dev.orion.entity.Activity;
import dev.orion.entity.User;

public interface StepExecutor {
    public void execute(Activity activity, User user);

    public String getStepRepresentation();
}
