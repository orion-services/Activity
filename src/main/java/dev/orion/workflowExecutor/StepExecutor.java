package dev.orion.workflowExecutor;

import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.Activity;
import dev.orion.entity.Step;
import dev.orion.entity.User;

public interface StepExecutor {
    public <T extends Step> void execute(Activity activity, User user, T step);

    public <T extends Step> void validate(Activity activity, User user, T step) throws NotValidActionException;

    public String getStepRepresentation();
}
