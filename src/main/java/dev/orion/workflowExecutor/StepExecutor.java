package dev.orion.workflowExecutor;

import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;

public interface StepExecutor {
    <T extends Step> void execute(Document document, User user, T step);

    <T extends Step> void validate(Document document, User user, T step) throws NotValidActionException;

    <T extends Step> boolean isFinished(Activity activity, Document document, T step) throws NotValidActionException;

    String getStepRepresentation();
}
