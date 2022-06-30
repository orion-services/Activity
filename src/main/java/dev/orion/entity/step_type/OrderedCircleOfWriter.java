package dev.orion.entity.step_type;

import dev.orion.commom.constant.CircularStepFlowDirectionTypes;
import dev.orion.entity.Step;
import lombok.*;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Entity
@Getter
@Setter
@DiscriminatorValue("ORDERED_CIRCLE_OF_WRITERS")
@NoArgsConstructor
@RequiredArgsConstructor
public class OrderedCircleOfWriter extends Step {
    final public String type = "ORDERED_CIRCLE_OF_WRITERS";
    final public String description = "Following the Circle of Learners, this step ir circular and respect a sequence order from the flow direction";

    @NonNull
    @Column(nullable = false)
    private CircularStepFlowDirectionTypes flowDirection;

    private Integer rounds = 1;
}
