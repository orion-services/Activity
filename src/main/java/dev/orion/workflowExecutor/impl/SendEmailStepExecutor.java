package dev.orion.workflowExecutor.impl;

import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import dev.orion.entity.step_type.CircleOfWriters;
import dev.orion.entity.step_type.SendEmail;
import dev.orion.workflowExecutor.StepExecutor;

import javax.enterprise.context.ApplicationScoped;

@ApplicationScoped
public class SendEmailStepExecutor implements StepExecutor {
    @Override
    public void execute(Document document, User user, Step step) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public void validate(Document document, User user, Step step) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public boolean isFinished(Activity activity, Step step) throws NotValidActionException {
        return false;
    }

    @Override
    public void validateConfig(Stage stage) {

    }

    @Override
    public String getStepRepresentation() {
        return new SendEmail().getStepType();
    }
}
