package dev.orion.entity.step_type;

import dev.orion.commom.constant.CircularStepFlowDirectionTypes;
import dev.orion.entity.Step;
import lombok.*;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Entity
@Getter
@DiscriminatorValue("CIRCULAR")
@NoArgsConstructor
@RequiredArgsConstructor
public class CircleOfWriters extends Step {
    @Column(updatable = false)
    public String type = "CIRCULAR";
    public String description = "Following the Circle of Learners, this step should create a circular activity";

    @Setter
    @NonNull
    private CircularStepFlowDirectionTypes flowDirection;

}
