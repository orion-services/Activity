package dev.orion.entity.step_type;

import dev.orion.commom.enums.CircularStepFlowDirectionTypes;
import dev.orion.entity.Step;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Entity
@Getter
@DiscriminatorValue("SNOW_BALL")
public class ReverseSnowball extends Step {
    private static final String TYPE = "REVERSE SNOWBALL";
    private static final String DESCRIPTION = "Another skill";

    public ReverseSnowball() {
        super(TYPE, DESCRIPTION);
    }
}
