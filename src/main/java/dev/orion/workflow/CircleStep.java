package dev.orion.workflow;

import dev.orion.entity.Activity;
import dev.orion.entity.User;
import dev.orion.entity.step_type.Circular;

public class CircleStep implements StepExecutor {
    @Override
    public void execute(Activity activity, User user) {
        throw new RuntimeException("Should implement it");
    }

    @Override
    public String getStepRepresentation() {
        return new Circular().getStepType();
    }
}
