package dev.orion.workflowExecutor;

import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import dev.orion.entity.step_type.ReverseSnowball;

import javax.enterprise.context.ApplicationScoped;

@ApplicationScoped
public class ReverseSnowBallStepExecutor implements StepExecutor {
    @Override
    public void execute(Document document, User user, Step step) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public void validate(Document document, User user, Step step) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public <T extends Step> boolean isFinished(Activity activity, T step) throws NotValidActionException {
        return false;
    }

    @Override
    public String getStepRepresentation() {
        return new ReverseSnowball().getStepType();
    }
}
