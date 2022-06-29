package dev.orion.workflowExecutor;

import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.Document;
import dev.orion.entity.Step;
import dev.orion.entity.User;
import dev.orion.entity.step_type.CircleOfWriters;

import javax.enterprise.context.ApplicationScoped;

@ApplicationScoped
public class CircleStepExecutor implements StepExecutor {
    @Override
    public void execute(Document document, User user, Step step) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public void validate(Document document, User user, Step step) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public <T extends Step> boolean isFinished(Document document, User user, T step) throws NotValidActionException {
        return false;
    }

    @Override
    public String getStepRepresentation() {
        return new CircleOfWriters().getStepType();
    }
}
