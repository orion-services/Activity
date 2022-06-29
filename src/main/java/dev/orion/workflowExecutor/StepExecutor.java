package dev.orion.workflowExecutor;

import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.Document;
import dev.orion.entity.Step;
import dev.orion.entity.User;

public interface StepExecutor {
    <T extends Step> void execute(Document document, User user, T step);

    <T extends Step> void validate(Document document, User user, T step) throws NotValidActionException;

    <T extends Step> boolean isFinished(Document document, User user, T step) throws NotValidActionException;

    public String getStepRepresentation();
}
