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
@DiscriminatorValue("CIRCULAR")
@NoArgsConstructor
@RequiredArgsConstructor
public class CircleOfWriters extends Step {
    @Column(updatable = false)
    final public String type = "CIRCULAR";

    @Column(updatable = false)
    final public String description = "Following the Circle of Learners, this step should create a circular activity";

    @NonNull
    private CircularStepFlowDirectionTypes flowDirection;


    private boolean isSingleDocumentByUser = false;

}
