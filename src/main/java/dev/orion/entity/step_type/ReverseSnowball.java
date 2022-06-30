package dev.orion.entity.step_type;

import dev.orion.entity.Step;
import lombok.Getter;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Entity
@Getter
@DiscriminatorValue("SNOW_BALL")
public class ReverseSnowball extends Step {
    private static final String TYPE = "REVERSE SNOWBALL";
    private static final String DESCRIPTION = "Another skill";
}
