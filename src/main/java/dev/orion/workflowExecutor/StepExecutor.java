package dev.orion.workflowExecutor;

import dev.orion.commom.exceptions.NotValidActionException;
import dev.orion.entity.Activity;
import dev.orion.entity.User;

public interface StepExecutor {
    public void execute(Activity activity, User user);

    public void validate(Activity activity, User user) throws NotValidActionException;

    public String getStepRepresentation();
}
