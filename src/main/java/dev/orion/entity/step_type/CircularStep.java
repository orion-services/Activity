package dev.orion.entity.step_type;

import dev.orion.entity.Activity;
import dev.orion.entity.StepBase;
import dev.orion.util.workflow_yaml.CircularStepFlowDirectionTypes;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Entity
@Getter
@Setter
@DiscriminatorValue("CIRCULAR")
public class CircularStep extends StepBase {
    private static final String NAME = "CIRCULAR";
    private static final String DESCRIPTION = "Following the Circle of Learners, this step should create a circular activity";

    @Column(nullable = false)
    private CircularStepFlowDirectionTypes flowDirection;

    public CircularStep() {
        super(NAME, DESCRIPTION);
    }
    @Override
    public void action(Activity activity) {

    }
}
