package dev.orion.workflowExecutor;

import dev.orion.entity.Activity;
import dev.orion.entity.User;
import dev.orion.entity.step_type.CircleOfWriters;

import javax.enterprise.context.ApplicationScoped;

@ApplicationScoped
public class CircleStepExecutor implements StepExecutor {
    @Override
    public void execute(Activity activity, User user) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public void validate(Activity activity, User user) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public String getStepRepresentation() {
        return new CircleOfWriters().getStepType();
    }
}
