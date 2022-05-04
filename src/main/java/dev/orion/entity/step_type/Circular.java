package dev.orion.entity.step_type;

import dev.orion.commom.enums.CircularStepFlowDirectionTypes;
import dev.orion.entity.Step;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Entity
@Getter
@DiscriminatorValue("CIRCULAR")
public class Circular extends Step {
    private static final String TYPE = "CIRCULAR";
    private static final String DESCRIPTION = "Following the Circle of Learners, this step should create a circular activity";

    @Setter
    private CircularStepFlowDirectionTypes flowDirection;

    public Circular() {
        super(TYPE, DESCRIPTION);
    }

}
