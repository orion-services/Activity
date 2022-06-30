package dev.orion.workflowExecutor;

import dev.orion.commom.exception.IncompleteWorkflowException;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import lombok.val;

public interface StepExecutor {
     void execute(Document document, User user, Step step);

    void validate(Document document, User user, Step step) throws NotValidActionException;

    boolean isFinished(Activity activity, Step step) throws NotValidActionException;

    void validateConfig(Stage stage);

    String getStepRepresentation();
}
