package dev.orion.workflowExecutor;

import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;

public interface StepExecutor {
     void execute(Document document, User user, Step step);

    void validate(Document document, User user, Step step) throws NotValidActionException;

    boolean isFinished(Activity activity, Step step) throws NotValidActionException;

    void validateConfig(Stage stage) throws NotValidActionException;

    String getStepRepresentation();
}
